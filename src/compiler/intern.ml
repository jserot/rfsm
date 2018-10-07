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

(* Abstract syntax -> internal models *)

open Syntax
   
(* Types from type expressions *)

exception Unbound_expr_index of string * type_index_expr 

let type_index_of_index_expr e =
  let rec type_index_of = function 
    TEConst c -> Types.Index.TiConst c
  | TEVar v -> Types.Index.TiVar v
  | TEBinop (op,e1,e2) -> Types.Index.TiBinop (op, type_index_of e1, type_index_of e2) in
  type_index_of e.ti_desc
    
exception Unbound_type_ctor of string
                             
let rec type_of_type_expr tenv te = match te with
  | TEBool -> Types.TyBool
  | TEInt None -> Types.TyInt None
  | TEFloat -> Types.TyFloat
  | TEInt (Some (lo,hi)) -> Types.TyInt (Some (type_index_of_index_expr lo, type_index_of_index_expr hi))
  | TEEvent -> Types.TyEvent
  | TEName n ->
     begin
       try List.assoc n tenv.Typing.te_defns
       with Not_found -> raise (Unbound_type_ctor n)
     end
  | TEArray (sz, te') -> TyArray (type_index_of_index_expr sz, type_of_type_expr tenv te')

and type_of_type_expression tenv te = type_of_type_expr tenv te.te_desc


let mk_bool_expr e = match e.Expr.e_desc with
    | Expr.EInt 0 -> { e with Expr.e_desc = EBool false }
    | Expr.EInt 1 -> { e with Expr.e_desc = EBool true }
    | _ -> e 

let mk_fsm_model tenv { fsm_desc = f; fsm_loc = loc } = 
  let mk_typed what (id,te) =
    if List.mem_assoc id tenv.Typing.te_ctors
    then Error.warning (Printf.sprintf "declaration of %s %s in FSM model %s shadows enum value" what id f.fd_name);
    id, type_of_type_expression tenv te in 
  let local_types =
      List.map (function (dir,(id,te)) -> (id, type_of_type_expression tenv te)) f.fd_ios
    @ List.map (function (id,te) -> id, type_of_type_expression tenv te) f.fd_vars in
  let type_of id =
    try List.assoc id local_types
    with Not_found -> failwith ("Intern.mk_fsm_model: cannot retrieve type for identifier " ^ id) in
  let pp_action a = match a with
    (* Replace all assignations [v:=0/1], where [v:bool] by [v:=false/true] *)
    | Action.Assign ({l_desc=Var0 v}, e) ->
       begin
         match type_of v with
         | Types.TyBool -> Action.Assign ({l_desc=Var0 v}, mk_bool_expr e)
         | _ -> a
       end
    | Action.Assign ({l_desc=Var1 (v,i)}, e) ->
       begin
         match type_of v with
         | Types.TyArray (_, Types.TyBool) -> Action.Assign ({l_desc=Var1 (v,i)}, mk_bool_expr e)
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
    | _ -> Error.fatal_error "Intern.mk_fsm_model" in
  let mk_prio p = if p then 1 else 0 in
  let mk_act a = pp_action a.act_desc in
  Fsm.build_model
    ~name:f.fd_name
    ~states:f.fd_states
    ~params:(List.map (mk_typed "parameter") f.fd_params)
    ~ios:(List.map (function (dir,desc) -> let id,ty = mk_typed "input/output" desc in dir,id,ty) f.fd_ios)
    ~vars:(List.map (mk_typed "variable") f.fd_vars)
    ~trans:(List.map (function (s,cond,acts,s',p) -> s, mk_cond cond, List.map mk_act acts, s', mk_prio p) f.fd_trans)
    ~itrans:(let q0,acts = f.fd_itrans in q0, List.map mk_act acts)

let mk_fsm_inst tenv models globals { fi_desc=f; fi_loc=loc } = 
  let model =
    try List.find (function m -> m.Fsm.fm_name = f.fi_model) models
    with Not_found -> Error.unbound_fsm loc f.fi_model in  
  let params =
    try List.map2 (fun (p,ty) v -> p, v) model.Fsm.fm_params f.fi_params
    with Invalid_argument _ -> Error.fsm_mismatch "parameter(s)" loc f.fi_name in
  let mk_global id =
    try List.assoc id globals
    with Not_found -> Error.unbound_global loc id in
  Fsm.build_instance
      ~tenv:tenv
      ~name:f.fi_name
      ~model:model
      ~params:params
      ~ios:(List.map mk_global f.fi_args)

let mk_bool_val v = match v with
    | Expr.Val_int 0 -> Expr.Val_bool false
    | Expr.Val_int 1 -> Expr.Val_bool true
    | _ -> v

let pp_value_change ty (t,v) = match ty with
  | Types.TyBool -> t, mk_bool_val v
  | _ -> t, v

let mk_stim_desc ty = function
  | Periodic (p,t1,t2) -> Fsm.Periodic (p,t1,t2)
  | Sporadic ts -> Fsm.Sporadic ts
  | ValueChange vcs -> Fsm.ValueChange (List.map (pp_value_change ty) vcs)
                     
let mk_global tenv { g_desc = g; g_loc = loc } = 
  let ty = type_of_type_expression tenv g.gd_type in
  match g.gd_desc with
  | GInp stim -> g.gd_name, Fsm.GInp (g.gd_name, ty, mk_stim_desc ty stim.stim_desc)
  | GOutp -> g.gd_name, Fsm.GOutp (g.gd_name, ty)
  | GShared -> g.gd_name, Fsm.GShared (g.gd_name, ty)

let mk_type_defn tenv { td_desc = d; td_loc = loc } =
  let open Typing in
  match d with
  | TD_Alias (id, te) ->
     { tenv with te_defns = (id, type_of_type_expr tenv te) :: tenv.te_defns }
  | TD_Enum (id, cs) ->
     let ty = Types.TyEnum cs in
     { tenv with te_defns = (id, ty) :: tenv.te_defns;
                 te_ctors = List.map (function c -> c, ty) cs @ tenv.te_ctors }

let type_of_function tenv fd =
  let ty_args = List.map (function (id,te) -> id, type_of_type_expression tenv te) fd.ff_args in
  let tenv' = { tenv with te_vars = ty_args @ tenv.te_vars } in
  let ty_body =
    try Typing.type_expression tenv' fd.ff_body.e_desc
    with Typing.Typing_error (_,t,t') -> raise (Typing.Type_error ("body", "function \"" ^ fd.ff_name ^ "\"", t, t')) in
  let ty_result = type_of_type_expression tenv fd.ff_res in
  begin try Types.unify ty_body ty_result
  with Types.TypeConflict _ -> raise (Typing.Type_error ("result", "function \"" ^ fd.ff_name ^ "\"", ty_body, ty_result)) end;
  Types.TyArrow (Types.TyProduct (List.map snd ty_args), ty_result)

let mk_fn_defn tenv { fd_desc = fd } =
  { tenv with Typing.te_vars = (fd.ff_name, type_of_function tenv fd) :: tenv.te_vars }

let mk_global_fn tenv { fd_desc = fd } =
    let ty = List.assoc fd.ff_name tenv.Typing.te_vars in
    fd.ff_name, (ty, Sysm.MFun (List.map fst fd.ff_args, fd.ff_body.e_desc))
                      
let build_system name p = 
  let tenv = List.fold_left mk_type_defn Typing.builtin_tenv p.p_type_decls in
  let tenv' = List.fold_left mk_fn_defn tenv p.p_fn_decls in
  (* let _ = Typing.dump_tenv tenv' in *)
  let models = List.map (mk_fsm_model tenv') p.p_fsm_models in
  let globals = List.map (mk_global tenv') p.p_globals in
  let gfns = List.map (mk_global_fn tenv') p.p_fn_decls in
  let fsms = List.map (mk_fsm_inst tenv' models globals) p.p_fsm_insts in
  let m = Sysm.build name gfns fsms in
  (* let _ = Sysm.dump stdout m in *)
  m
