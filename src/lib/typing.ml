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
   
exception Unbound_id of string * string 
exception Typing_error of Expr.t * Types.typ * Types.typ
exception Type_error of string * string * Types.typ * Types.typ (** what, where, type, type *)
exception Internal_error of string (** where *)
exception Illegal_cast of Expr.t
exception Invalid_record_access of Expr.t

type tenv =
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_rfields: (string * typ) list;
    te_defns: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

let lookup_type what env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id (what, id))

let lookup_type_scheme env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id ("builtin operator", id))

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
  
(* Typing expressions *)
                                    
let rec type_expression tenv expr =
  let unify t1 t2 =
    try Types.unify t1 t2
    with
    | Types.TypeConflict _
    | Types.TypeCircularity _ ->
      raise (Typing_error (expr, t1, t2)) in   
  let type_expr expr = match expr.Expr.e_desc with
    Expr.EInt c -> type_int []
  | Expr.EFloat b -> TyFloat
  | Expr.EChar b -> TyChar
  | Expr.EBool b -> TyBool
  | Expr.EVar id -> lookup_type "variable" tenv.te_vars id
  | Expr.EEnum c ->  lookup_type "enum value" tenv.te_ctors c
  | Expr.EBinop (op,e1,e2) ->
      let ty_fn = type_instance (lookup_type_scheme tenv.te_prims op) in
      type_application expr tenv ty_fn [e1;e2] 
  | Expr.EFapp (f,es) ->
      let tenv' = tenv.te_vars @ List.map (function id, ts -> id, Types.type_instance ts) tenv.te_prims in 
      let ty_fn = lookup_type "function" tenv' f in
      type_application expr tenv ty_fn es
  | Expr.ECond (e1,e2,e3) ->
      let ty_e1 = type_expression tenv e1 in
      let ty_e2 = type_expression tenv e2 in
      let ty_e3 = type_expression tenv e3 in
      unify ty_e1 TyBool;
      unify ty_e2 ty_e3;
      ty_e2
  | Expr.EArr (a,idx) ->
     let ty_arg = lookup_type "array or int" tenv.te_vars a in
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
     let ty_arg = lookup_type "int" tenv.te_vars a in
     let ty_idx = type_expression tenv idx in
     unify ty_idx (type_int []);
     unify ty_arg (type_int []);
     type_int [1]
  | Expr.EBitrange (a,idx1,idx2) ->
     let ty_arg = lookup_type "int" tenv.te_vars a in
     let ty_idx1 = type_expression tenv idx1 in
     let ty_idx2 = type_expression tenv idx2 in
     unify ty_idx1 (type_int []);
     unify ty_idx2 (type_int []);
     unify ty_arg (type_int []);
     type_int []
  | Expr.ERecord (a,f) ->
     begin
       match lookup_type "record" tenv.te_vars a with
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
  | TypeCircularity(t,t') -> raise (Typing_error (expr, t, t'))

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
