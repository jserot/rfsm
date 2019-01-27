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

open Types
   
exception Undef_symbol of string * string * string (** where, what, name *)
exception Type_error of Expr.t * Types.typ * Types.typ
exception Typing_error of string * string * Types.typ * Types.typ (** what, where, type, type *)
exception Illegal_cast of Expr.t
exception Invalid_record_access of Expr.t

type tenv =
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_rfields: (string * typ) list;
    te_defns: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

let lookup_type where what env id =
  try List.assoc id env
  with Not_found -> raise (Undef_symbol (where, what, id))

let lookup_type_scheme where env id =
  try List.assoc id env
  with Not_found -> raise (Undef_symbol (where, "builtin operator", id))

(* Typing type expressions *)

let type_index_of_index_expr e =
  let open Type_expr in
  let rec type_index_of = function 
    TEConst c -> Types.Index.TiConst c
  | TEVar v -> Types.Index.TiVar v
  | TEBinop (op,e1,e2) -> Types.Index.TiBinop (op, type_index_of e1, type_index_of e2) in
  type_index_of e

exception Unbound_type_ctor of string
                             
let rec type_of_type_expr tenv texpr =
  let open Type_expr in 
  let rec type_texpr te = match te.te_desc with
  | TEBool -> Types.TyBool
  | TEInt TA_none -> TyInt (new_size_var())
  | TEInt (TA_size sz) -> Types.TyInt (Types.SzExpr1 (type_index_of_index_expr sz))
  | TEInt (TA_range (lo,hi)) -> Types.TyInt (SzExpr2 (type_index_of_index_expr lo, type_index_of_index_expr hi))
  | TEFloat -> Types.TyFloat
  | TEChar -> Types.TyChar
  | TEEvent -> Types.TyEvent
  | TEName n ->
     begin
       try List.assoc n tenv.te_defns
       with Not_found -> raise (Unbound_type_ctor n)
     end
  | TEArray (sz, te') -> TyArray (type_index_of_index_expr sz, type_of_type_expr tenv te') in
  let ty = Types.real_type (type_texpr texpr) in
  texpr.te_typ <- ty;
  ty
  
(* (\* Typing values *\)
 * 
 * let type_value tenv v = 
 *   let lookup_typ = lookup_type ("value \"" ^ Expr.string_of_value v ^ "\"") in
 *   match v with
 *   | Expr.Val_int _ -> type_int []
 *   | Expr.Val_float _ -> TyFloat
 *   | Expr.Val_char _ -> TyChar
 *   | Expr.Val_bool _ -> TyBool
 *   | Expr.Val_enum c -> lookup_typ "enum value" tenv.te_ctors c
 *   | Expr.Val_array vs when Array.length vs > 0 -> 
 *       let ty1 = type_value tenv vs.(0) in
 *       for i=1 to Array.length vs - 1 do
 *         unify ty1 (type_value tenv vs.(i))
 *       done;
 *       TyArray (TiConst (List.length vs'), ty1)
 *   | Expr.Val_record ((f1::fs) as fs') ->
 *       let lookup f = lookup_typ "record field" tenv.te_rfields f in
 *       let ty1 = lookup f1 in
 *       List.iter (fun v -> unify ty1 (lookup f)) fs;
 *       ty1
 *   | _ -> Misc.fatal_error ("Typing.type_value (" ^ Expr.string_of_value v ^ ")") *)
                                    
(* Typing expressions *)
                                    
let rec type_expression tenv expr =
  let unify t1 t2 =
    try Types.unify t1 t2
    with
    | Types.TypeConflict _
    | Types.TypeCircularity _ ->
      raise (Type_error (expr, t1, t2)) in   
  let lookup_typ = lookup_type ("expression \"" ^ Expr.to_string expr ^ "\"") in
  let lookup_typ_scheme = lookup_type_scheme ("expression \"" ^ Expr.to_string expr ^ "\"") in
  let type_expr expr = match expr.Expr.e_desc with
    Expr.EInt c -> type_int []
  | Expr.EFloat b -> TyFloat
  | Expr.EChar b -> TyChar
  | Expr.EBool b -> TyBool
  | Expr.EVar id -> lookup_typ "variable" tenv.te_vars id
  | Expr.EEnum c ->  lookup_typ "enum value" tenv.te_ctors c
  | Expr.EBinop (op,e1,e2) ->
      let ty_fn = type_instance (lookup_typ_scheme tenv.te_prims op) in
      type_application expr tenv ty_fn [e1;e2] 
  | Expr.EFapp (f,es) ->
      let tenv' = tenv.te_vars @ List.map (function id, ts -> id, Types.type_instance ts) tenv.te_prims in 
      let ty_fn = lookup_typ "function" tenv' f in
      type_application expr tenv ty_fn es
  | Expr.ECond (e1,e2,e3) ->
      let ty_e1 = type_expression tenv e1 in
      let ty_e2 = type_expression tenv e2 in
      let ty_e3 = type_expression tenv e3 in
      unify ty_e1 TyBool;
      unify ty_e2 ty_e3;
      ty_e2
  | Expr.EArrExt [] -> Misc.fatal_error "Typing.type_expression: empty array" (* should not happen *)
  | Expr.EArrExt ((e1::es) as exps) -> 
      let ty_e1 = type_expression tenv e1 in
      List.iter (function e -> unify ty_e1 (type_expression tenv e)) es;
      TyArray (TiConst (List.length exps), ty_e1)
  | Expr.EArr (a,idx) ->
     let ty_arg = lookup_typ "array or int" tenv.te_vars a in
     let ty_idx = type_expression tenv idx in
     unify ty_idx (type_int []);
     begin match ty_arg with
     | TyInt _ ->  (* Special case *)
        expr.Expr.e_desc <- EBit (a,idx);  (* This is a hack.. *)
        type_int [1]
     | _ -> 
        let ty_res = new_type_var () in
        unify ty_arg (TyArray(TiConst (size_of ty_arg), ty_res));
        Types.real_type ty_res
     end 
  | Expr.EBit (a,idx) ->
     let ty_arg = lookup_typ "int" tenv.te_vars a in
     let ty_idx = type_expression tenv idx in
     unify ty_idx (type_int []);
     unify ty_arg (type_int []);
     type_int [1]
  | Expr.EBitrange (a,idx1,idx2) ->
     let ty_arg = lookup_typ "int" tenv.te_vars a in
     let ty_idx1 = type_expression tenv idx1 in
     let ty_idx2 = type_expression tenv idx2 in
     unify ty_idx1 (type_int []);
     unify ty_idx2 (type_int []);
     unify ty_arg (type_int []);
     type_int []
  | Expr.ERecord (a,f) ->
     begin
       match lookup_typ "record" tenv.te_vars a with
       | TyRecord (_,fs) ->
          begin
            try List.assoc f fs
            with Not_found -> raise (Invalid_record_access expr)
          end
       | _ -> raise (Invalid_record_access expr)
     end 
  | Expr.ECast (e,te) ->
      let ty_e = type_expression tenv e in
      let ty_t = type_of_type_expr tenv te in
      type_cast e ty_e ty_t in
  let ty = Types.real_type (type_expr expr) in
  (* Printf.printf "** Typing.type_expression(%s) = %s\n" (Expr.string_of_expr expr.e_desc) (Types.string_of_type ty); flush stdout; *)
  expr.e_typ <- ty;
  ty

and type_application expr tenv ty_fn args =
  let ty_arg = TyProduct (List.map (type_expression tenv) args) in
  let ty_result = new_type_var () in
  try 
    unify ty_fn (TyArrow (ty_arg,ty_result));
    real_type ty_result
  with
    TypeConflict (t,t')
  | TypeCircularity(t,t') -> raise (Type_error (expr, t, t'))

and type_cast e t1 t2 = match t1, t2 with
  | TyInt _, TyInt _
  | TyInt _, TyBool
  | TyInt _, TyChar
  | TyChar, TyInt _
  | TyInt _, TyFloat
  | TyBool, TyBool
  | TyBool, TyInt _
  | TyFloat, TyFloat
  | TyFloat, TyInt _ -> t2
  | _, _ -> raise (Illegal_cast e)

let type_check what where ty ty'  =
  try
    Types.unify ty ty'
  with
    Types.TypeConflict _ -> raise (Typing_error (what, where, ty, ty'))

(* Typing FSM models *)

let type_of_list = function
  | [t] -> t
  | ts -> Types.TyProduct ts

let types_of_fsm_model f = 
  (* Computes the "local" typing environment associated to an FSM model, containing
     the types of parameters, inputs, outputs and local variables *)
  let open Fsm in
  let vars =
      f.fm_params
    @ f.fm_vars
    @ List.map (function (id, (_, ty)) -> id, ty) f.fm_ios  in
  (* let ctors =
   *   Misc.collect_assoc
   *     (fun (id,ty) -> Types.enums_of ty)
   *     vars in
   * { te_vars=vars; te_ctors=ctors; te_rfields=[]; te_defns=[]; te_prims=[] } *)
  vars

let type_check_index_expression name tenv exp =
  try type_check
        ("index expression \"" ^ (Expr.to_string exp) ^ "\"") ("FSM \"" ^ name ^ "\"")
        (type_expression tenv exp) (Types.type_int [])
  with
  | Type_error (expr, ty, ty') ->
     raise (Typing_error ("index expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ name ^ "\"", ty, ty'))

let type_fsm_lhs name tenv lhs =
  let open Action in
  begin match lhs.l_desc with
  | LhsVar v -> lookup_type name "variable" tenv.te_vars v
  | LhsArrInd (a,i) ->
     type_check_index_expression name tenv i;
     begin match lookup_type name "variable" tenv.te_vars a with
     | TyInt _ -> (* Special case *)
        lhs.l_desc <- LhsArrRange (a,i,i);  (* This is a hack *)
        Types.type_int [1]
     | ty ->  (* Should be an array *)
        Types.subtype_of ty
     end
  | LhsArrRange (a,i1,i2) ->
     type_check_index_expression name tenv i1;
     type_check_index_expression name tenv i2;
     begin match lookup_type name "variable" tenv.te_vars a, i1.e_desc, i2.e_desc with
     | TyInt (Types.SzExpr1 (Types.Index.TiConst sz)), Expr.EInt hi, Expr.EInt lo when hi >= lo ->
        Types.type_int [hi-lo+1]  (* If x::int<n>, then x[hi:lo]::int<hi-lo+1> *)
     | TyInt _, _, _ ->           (* Cannot infer size otherwise  *)
        Types.type_int []
     | ty, _, _ -> 
        raise (Typing_error ("LHS \"" ^ Action.string_of_lhs lhs ^ "\"", "FSM \"" ^ name ^ "\"", ty, Types.type_int []))
     end
  | LhsRField (a,f) ->
     begin match lookup_type name "variable" tenv.te_vars a with
     | TyRecord (_,fs) -> List.assoc f fs 
     | _ -> Misc.fatal_error "Typing.type_fsm_lhs"
     end
  | exception _ -> Misc.fatal_error "Typing.type_fsm_lhs"
  end

let type_check_fsm_action name tenv act = match act with 
  | Action.Assign (lhs, exp) -> 
     let t = type_fsm_lhs name tenv lhs in
     let t' =
       try type_expression tenv exp
       with Type_error (expr, ty, ty') ->
         raise (Typing_error ("expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ name ^ "\"", ty, ty')) in
     begin
       try type_check
             ("action \"" ^ Action.to_string act ^ "\"")
             ("FSM \"" ^ name ^ "\"")
             t' t
       with
       | Type_error (expr, ty, ty') ->
          raise (Typing_error ("action \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ name ^ "\"", ty, ty'))
     end
  | Action.Emit s ->
     let t = lookup_type name "event" tenv.te_vars s in
     type_check ("action \"" ^ Action.to_string act ^ "\"") ("FSM \"" ^ name ^ "\"") t TyEvent
  | _ -> ()

let type_check_fsm_event name tenv ev =
    try type_check
          ("event \"" ^ ev ^ "\"") ("FSM \"" ^ name ^ "\"")
          (lookup_type name "event" tenv.te_vars ev) Types.TyEvent
    with
    | Type_error (expr, ty, ty') ->
       raise (Typing_error ("event \"" ^ ev ^ "\"", "FSM \"" ^ name ^ "\"", ty, ty'))

let type_check_fsm_guard name tenv gexp =
    try type_check
          ("guard \"" ^ (Condition.string_of_guard gexp) ^ "\"") ("FSM \"" ^ name ^ "\"")
          (type_expression tenv gexp) Types.TyBool
    with
    | Type_error (expr, ty, ty') ->
       raise (Typing_error ("guard expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ name ^ "\"", ty, ty'))

let type_check_fsm_condition name tenv (evs,gs) =
  begin match evs with
    [] -> ()
  | [e] -> type_check_fsm_event name tenv e
  | _ -> Misc.fatal_error "Typing.type_check_fsm_condition" (* should not happen *)
  end;
  List.iter (type_check_fsm_guard name tenv) gs

let type_check_fsm_state name r s =
  if not (List.mem s (Fsm.Repr.states' r))
   then raise (Fsm.Invalid_state (s,name))
                                
let type_check_fsm_transition name repr tenv (s,(cond,acts,_,_),s') =
  (* For each transition [s -> cond / acts -> s'] check that
     - [s] are [s'] are listed as states if [f] declaration
     - [cond] has form [e.[guard1]...[guardn]] where [e] has type [event] and each [guardi] type [bool]
     - for each [act]=[lhs:=rhs] in [acts], [type(rhs)=type(lhs)] *)
  type_check_fsm_state name repr s;
  type_check_fsm_state name repr s';
  type_check_fsm_condition name tenv cond;
  List.iter (type_check_fsm_action name tenv) acts

let type_check_fsm_itransition name repr tenv ((_,acts,_,_),s) =
  (* For the initial transitio [/ acts -> s'] check that
     - [s'] is listed as state if [f] declaration
     - for each [act]=[lhs:=rhs] in [acts], [type(rhs)=type(lhs)] *)
  type_check_fsm_state name repr s;
  List.iter (type_check_fsm_action name tenv) acts
          
let type_check_fsm_model tenv f =
  let open Fsm in
  List.iter (type_check_fsm_transition f.fm_name f.fm_repr tenv) (Fsm.Repr.transitions f.fm_repr);
  List.iter (type_check_fsm_itransition f.fm_name f.fm_repr tenv) (Fsm.Repr.itransitions f.fm_repr)

let type_fsm_model tenv f =
  (* Type checks an FSM model *)
  let open Fsm in
  let tenv = { tenv with te_vars = types_of_fsm_model f @ tenv.te_vars } in
  type_check_fsm_model tenv f

(* Typing FSM instances *)

let types_of_fsm_inst f = 
  (* Computes the "local" typing environment associated to an FSM instance, containing
     the types of parameters, inputs, outputs and local variables *)
  let open Fsm in
  let mk (id,(ty,_)) = id, ty in
    List.map mk f.f_params
  @ f.f_vars
  @ List.map mk (f.f_inps @ f.f_inouts @ f.f_outps)
  
let type_fsm_inst tenv f =
  (* Type checks an FSM instance *)
  let open Fsm in
  (* Check that all type indexes have been instanciated for IOs and local vars *)
   let check_type kind (id,ty) = 
     match Types.ivars_of ty with
     | [] -> ()
     | vs -> raise (Fsm.Uninstanciated_type_vars (f.f_name, kind, id, vs)) in
   let type_of (id,(ty,_)) = id, ty in
   List.iter (check_type "input") (List.map type_of f.f_inps);
   List.iter (check_type "output") (List.map type_of f.f_outps);
   List.iter (check_type "inout") (List.map type_of f.f_inouts);
   List.iter (check_type "variable") f.f_vars;
   (* Type check conditions and actions on transitions *)
   let tenv = { tenv with te_vars = types_of_fsm_inst f @ tenv.te_vars } in
   List.iter (type_check_fsm_transition f.f_name f.f_repr tenv) (Fsm.Repr.transitions f.f_repr);
   List.iter (type_check_fsm_itransition f.f_name f.f_repr tenv) (Fsm.Repr.itransitions f.f_repr)

(* Typing globals *)
  
let type_check_stim tenv id ty st = match st with
  | Global.ValueChange vcs ->
     List.iter
       (type_check "stimuli" ("input \"" ^ id ^ "\"") ty) 
       (List.map (function (_,v) -> v.Expr.v_typ) vcs)
  | _ ->
     ()

(* Typing environment *)
                           
let builtin_tenv = {
  te_vars = [];
  te_ctors = [
   "True", TyBool;
   "False", TyBool
   ];
  te_rfields = [];
  te_defns = [];
  te_prims = List.map (function (id,(ty,_)) -> id, ty) Builtins.env
  }

(* Printing *)

let dump_tenv oc tenv =  (* For debug only *)
  Printf.fprintf oc " { te.vars = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_vars);
  Printf.fprintf oc "   te.ctors = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_ctors);
  Printf.fprintf oc "   te.defns = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ "=" ^ string_of_type ty) ", " tenv.te_defns);
  Printf.fprintf oc "   te.prims = %s }\n"
    (Utils.ListExt.to_string (function (id,ts) -> id ^ ":" ^ string_of_type_scheme ts) ", " tenv.te_prims)

