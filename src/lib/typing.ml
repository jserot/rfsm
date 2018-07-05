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

type tenv =
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_defns: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

let lookup_type what env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id (what, id))

let lookup_type_scheme env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id ("builtin operator", id))
                  
let rec type_expression tenv expr =
  let unify t1 t2 =
    try Types.unify t1 t2
    with
    | Types.TypeConflict _
    | Types.TypeCircularity _ ->
      raise (Typing_error (expr, t1, t2)) in   
  match expr with
    Expr.EInt c -> TyInt None
  | Expr.EFloat b -> TyFloat
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

and type_application expr tenv ty_fn args =
      let ty_arg = TyProduct (List.map (type_expression tenv) args) in
      let ty_result = new_type_var () in
      try 
          unify ty_fn (TyArrow (ty_arg,ty_result));
          real_type ty_result
      with
           TypeConflict (t,t')
         | TypeCircularity(t,t') -> raise (Typing_error (expr, t, t'))

let builtin_tenv = {
  te_vars = [];
  te_ctors = [
   "True", TyBool;
   "False", TyBool
   ];
  te_defns = [];
  te_prims = List.map (function (id,(ty,_)) -> id, ty) Builtins.env
  }

(* Printing *)

let dump_tenv tenv =  (* For debug only *)
  Printf.printf "te.vars = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_vars);
  Printf.printf "te.ctors = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_ctors);
  Printf.printf "te.defns = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ "=" ^ string_of_type ty) ", " tenv.te_defns);
  Printf.printf "te.prims = %s\n"
    (Utils.ListExt.to_string (function (id,ts) -> id ^ ":" ^ string_of_type_scheme ts) ", " tenv.te_prims)
