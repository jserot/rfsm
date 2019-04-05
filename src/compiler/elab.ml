(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

open Syntax

let type_of_type_expression tenv te = Typing.type_of_type_expr tenv te.te_desc

let rec mk_bool_expr e = match e.Expr.e_desc with
    | Expr.EInt 0 -> { e with Expr.e_desc = EBool false }
    | Expr.EInt 1 -> { e with Expr.e_desc = EBool true }
    | Expr.ECond (c,e1,e2) -> { e with Expr.e_desc = ECond (c, mk_bool_expr e1, mk_bool_expr e2) }
    | _ -> e 

let mk_fsm_model tenv { fsm_desc = f; fsm_loc = loc } = 
  let mk_typed what (id,te) =
    if List.mem_assoc id tenv.Typing.te_ctors
    then Misc.warning (Printf.sprintf "declaration of %s %s in FSM model %s shadows enum value" what id f.fd_name);
    id, type_of_type_expression tenv te in 
  let local_types =
      List.map (function (dir,(id,te)) -> (id, type_of_type_expression tenv te)) f.fd_ios
    @ List.map (function (id,te) -> id, type_of_type_expression tenv te) f.fd_vars in
  let type_of id =
    try List.assoc id local_types
    with Not_found -> Misc.fatal_error ("Static.mk_fsm_model: cannot retrieve type for identifier " ^ id) in
  let pp_action a = match a with
    (* Replace all assignations [v:=0/1], where [v:bool] by [v:=false/true] *)
    | Action.Assign ({l_desc=LhsVar v}, e) ->
       begin
         match type_of v with
         | Types.TyBool -> Action.Assign ({l_desc=LhsVar v}, mk_bool_expr e)
         | _ -> a
       end
    | Action.Assign ({l_desc=LhsArrInd (v,i)}, e) ->
       begin
         match type_of v with
         | Types.TyArray (_, Types.TyBool) -> Action.Assign ({l_desc=LhsArrInd (v,i)}, mk_bool_expr e)
         | _ -> a
       end
    | _ -> a in
  let pp_expr e =
    let open Expr in
    match e.e_desc with
    (* Replace all bool expr [e op 0/1], where [e:bool] and [op] is [=] or [!=] by [e op false/true] *)
    | EBinop (op, ({ e_desc = EVar v } as e'), e'') when List.mem op ["="; "!="] ->  
       begin
         match type_of v with
         | TyBool -> { e with e_desc = EBinop (op, e', mk_bool_expr e'') }
         | _ -> e
       end
    | _ -> e in
  let mk_cond c = match c.cond_desc with
      [ev],guards -> ev, List.map pp_expr guards
    | _ -> Misc.fatal_error "Intern.mk_fsm_model" in
  let mk_prio p = if p then 1 else 0 in
  let mk_act a = pp_action a.act_desc in
  let m = Fsm.build_model
    ~name:f.fd_name
    ~states:f.fd_states
    ~params:(List.map (mk_typed "parameter") f.fd_params)
    ~ios:(List.map (function (dir,desc) -> let id,ty = mk_typed "input/output" desc in dir,id,ty) f.fd_ios)
    ~vars:(List.map (mk_typed "variable") f.fd_vars)
    ~trans:(List.map (function (s,cond,acts,s',p) -> s, mk_cond cond, List.map mk_act acts, s', mk_prio p) f.fd_trans)
    ~itrans:(let q0,acts = f.fd_itrans in q0, List.map mk_act acts) in
  Typing.type_fsm_model tenv m;
  m

exception Unbound_fsm of Location.location * string
exception Unbound_global of Location.location * string
exception Fsm_mismatch of string * Location.location * string

let mk_fsm_inst tenv cenv models globals { fi_desc=f; fi_loc=loc } = 
  let model =
    try List.find (function m -> m.Fsm.fm_name = f.fi_model) models
    with Not_found -> raise (Unbound_fsm (loc,f.fi_model)) in  
  let params =
    try List.map2 (fun (p,ty) e -> p, Eval.eval cenv e) model.Fsm.fm_params f.fi_params
    with Invalid_argument _ -> raise (Fsm_mismatch ("parameter(s)",loc,f.fi_name)) in
  let mk_global id =
    try List.assoc id globals
    with Not_found -> raise (Unbound_global (loc,id)) in
  let m = Fsm.build_instance
      ~name:f.fi_name
      ~model:model
      ~params:params
      ~ios:(List.map mk_global f.fi_args) in
  Typing.type_fsm_inst tenv m;
  m

let mk_bool_val v = match v.Expr.v_desc with
    | Expr.Val_int 0 -> Expr.mk_bool false
    | Expr.Val_int 1 -> Expr.mk_bool true
    | _ -> v

let pp_value_change ty (t,v) = match ty with
  | Types.TyBool -> t, mk_bool_val v
  | _ -> t, v

let mk_stim_desc ty sd = match sd with
  | Global.ValueChange vcs -> Global.ValueChange (List.map (pp_value_change ty) vcs)
  | _ -> sd
                     
let mk_global tenv { g_desc = g; g_loc = loc } = 
  let ty = type_of_type_expression tenv g.gd_type in
  match g.gd_desc with
  | GInp stim -> g.gd_name, Global.GInp (g.gd_name, ty, mk_stim_desc ty stim.stim_desc)
  | GOutp -> g.gd_name, Global.GOutp (g.gd_name, ty)
  | GShared -> g.gd_name, Global.GShared (g.gd_name, ty)

let mk_type_defn tenv { td_desc = d; td_loc = loc } =
  let open Typing in
  match d with
  | TD_Alias (id, te) ->
     { tenv with te_defns = (id, type_of_type_expr tenv te) :: tenv.te_defns }
  | TD_Enum (id, cs) ->
     let ty = Types.TyEnum (Types.NmLit id,cs) in
     { tenv with te_defns = (id, ty) :: tenv.te_defns;
                 te_ctors = List.map (function c -> c, ty) cs @ tenv.te_ctors }
  | TD_Record (id, fs) ->
     let ty = Types.TyRecord (Types.NmLit id, List.map (function (n,te) -> (n, type_of_type_expr tenv te)) fs) in
     { tenv with te_defns = (id, ty) :: tenv.te_defns;
                 te_rfields = List.map (function (n,_) -> n, ty) fs @ tenv.te_rfields }

let type_of_function tenv fd =
  let ty_args = List.map (function (id,te) -> id, type_of_type_expression tenv te) fd.ff_args in
  let tenv' = { tenv with te_vars = ty_args @ tenv.te_vars } in
  let ty_body =
    try Typing.type_expression tenv' fd.ff_body.e_desc
    with Typing.Typing_error (_,_,t,t') -> raise (Typing.Typing_error ("body", "function \"" ^ fd.ff_name ^ "\"", t, t')) in
  let ty_result = type_of_type_expression tenv fd.ff_res in
  begin try Types.unify ty_body ty_result
  with Types.TypeConflict _ -> raise (Typing.Typing_error ("result", "function \"" ^ fd.ff_name ^ "\"", ty_body, ty_result)) end;
  Types.TyArrow (Types.TyProduct (List.map snd ty_args), ty_result)

exception Incomplete_record of string * Expr.value * Types.typ 
                             
let rec retype_value ty v =
  let open Expr in
  match v.v_desc, ty with
  | Val_int _, Types.TyBool -> (* Special case *)
     ()
  | Val_array vs, Types.TyArray (_, ty') -> 
     Array.iter (retype_value ty') vs;
     Types.unify ty v.v_typ
  | Val_record vs, Types.TyRecord (_, fs) -> 
     if List.length vs = List.length fs
     then List.iter2 (fun (_,v') (_,ty') -> retype_value ty' v') vs fs
     else raise (Incomplete_record ("",v,ty));
     Types.unify ty v.v_typ
  | _, _ ->
     Types.unify ty v.v_typ

let type_of_constant tenv cd =
  try
    let ty = type_of_type_expression tenv cd.cc_typ in
    Types.unify ty cd.cc_val.Expr.v_typ;
    retype_value ty cd.cc_val; 
    ty
  with
  | Typing.Typing_error (_,_,t,t') 
  | Types.TypeConflict (t,t') -> raise (Typing.Typing_error ("expression", "constant \"" ^ cd.cc_name ^ "\"", t, t'))

          
let type_of_input tenv gd =
  let ty = type_of_type_expression tenv gd.gd_type in
  begin
    match gd.gd_desc with
      GInp { stim_desc = ValueChange vcs } -> 
       begin
         try List.iter (fun (_,v) -> retype_value ty v) vcs;
         with
         | Typing.Typing_error (_,_,t,t') 
         | Types.TypeConflict (t,t') -> raise (Typing.Typing_error ("stimulus", "input \"" ^ gd.gd_name ^ "\"", t, t'))
         | Incomplete_record (_,v,ty) -> raise (Incomplete_record ("input \"" ^ gd.gd_name ^ "\"", v, ty))
       end
    |  _ -> ()
  end;
  ty

let mk_cst_defn tenv { cst_desc = cd } =
  { tenv with Typing.te_vars = (cd.cc_name, type_of_constant tenv cd) :: tenv.te_vars }

let mk_fn_defn tenv { fd_desc = fd } =
  { tenv with Typing.te_vars = (fd.ff_name, type_of_function tenv fd) :: tenv.te_vars }

let mk_global_fn tenv { fd_desc = fd } =
    let ty = List.assoc fd.ff_name tenv.Typing.te_vars in
    fd.ff_name, (ty, Static.MFun (List.map fst fd.ff_args, fd.ff_body.e_desc))

let mk_global_cst tenv { cst_desc = cd } =
    let ty = List.assoc cd.cc_name tenv.Typing.te_vars in
    cd.cc_name, (ty, Static.MConst cd.cc_val)
                      
let is_global_type_defn (_,ty) = match ty with
  | Types.TyEnum (name, cs) when Types.is_lit_name name -> true
  | Types.TyRecord (name, fs) -> true
  | _ -> false

let mk_inp_defn tenv { g_desc = gd } = match gd.gd_desc with
  | GInp _ -> { tenv with Typing.te_vars = (gd.gd_name, type_of_input tenv gd) :: tenv.te_vars }
  | _ -> tenv
  
let mk_const_env env (id, (ty, desc)) = match desc with
  | Static.MConst v -> (id,v) :: env
  | _ -> env
             
let process name p = 
  let tenv =
       Typing.builtin_tenv
    |> Misc.fold_left mk_type_defn p.p_type_decls
    |> Misc.fold_left mk_cst_defn p.p_cst_decls
    |> Misc.fold_left mk_fn_defn p.p_fn_decls in
  let models = List.map (mk_fsm_model tenv) p.p_fsm_models in
  let _ = tenv |> Misc.fold_left mk_inp_defn p.p_globals in
  let globals = List.map (mk_global tenv) p.p_globals in
  let gtyps = List.rev (List.filter is_global_type_defn tenv.te_defns) in 
  let gcsts = List.map (mk_global_cst tenv) p.p_cst_decls in
  let cenv = List.fold_left mk_const_env [] gcsts in
  let gfns = List.map (mk_global_fn tenv) p.p_fn_decls in
  let fsms = List.map (mk_fsm_inst tenv cenv models globals) p.p_fsm_insts in
  let s = Static.build ~name ~gtyps ~gfns ~gcsts models fsms in
  let has_testbench = p.p_fsm_insts <> [] in
  s, has_testbench

(* let dot_output dir ?(dot_options=[]) ?(fsm_options=[]) r =
 *   let fnames = match r with
 *   | Models ms ->
 *       List.map
 *         (Fsm.dot_output_model ~dot_options:dot_options ~options:fsm_options ~dir:dir)
 *         ms
 *   | System s ->
 *      Static.dot_output dir ~dot_options ~fsm_options s in
 *   List.iter Logfile.write fnames
 * 
 * let dump oc r = match r with
 *   | Models ms -> List.iter (Fsm.dump_model oc) ms
 *   | System s -> Static.dump oc s *)
