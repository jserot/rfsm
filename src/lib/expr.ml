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

type t = 
    EInt of int
  | EBool of bool
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  (* | ECond of test * t * t        (\** e1 ? e2 : e3 *\) *)

(* and test = t * string * t     (\** e1 relop e2 *\) *)

type e_val = 
  | Val_int of int
  | Val_bool of bool
  | Val_enum of string

(* type env = (string * e_val) list *)

(* module Builtins = struct 
 * 
 *   let binops = [
 *     "+", (+);
 *     "-", (-);
 *     "*", ( * ); 
 *     "/", ( / );
 *     "mod", ( mod ) 
 *     ]
 *   
 *   let relops = [
 *     "=", (=);
 *     "!=", (<>);
 *     "<", (<);
 *     ">", (>);
 *     "<=", (<=);
 *     ">=", (>=)
 *   ]
 *   
 *   exception Illegal_op of string
 *   
 *   let lookup ops op = 
 *     try List.assoc op ops
 *     with Not_found -> raise (Illegal_op op)
 * end *)


let of_value = function
    Val_int v -> EInt v
  | Val_bool b -> EBool b
  | Val_enum c -> EEnum c

let unset_event = None
let set_event = Some (Val_int 1)

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
let rec vars_of expr =
  match expr with
    EVar v -> VarSet.singleton v
  | EBinop (_,e1,e2) -> VarSet.union (vars_of e1) (vars_of e2)
  (* | ECond ((e11,op,e12),e2,e3) ->
   *    List.fold_left (fun acc e -> VarSet.union acc (vars_of e)) VarSet.empty [e11;e12;e2;e3] *)
  | ECond (e1,e2,e3) ->
     List.fold_left (fun acc e -> VarSet.union acc (vars_of e)) VarSet.empty [e1;e2;e3]
  | _ -> VarSet.empty
       
(* Substitution *)
                
(* Cannot be defined here because module Expr cannot depend on module Builtins ! *)
       
(* let rec subst vs expr = match expr with
 *   (\* Substitute each occurence of variables in [vs] by its value in [expr] *\)
 *   | EVar v when List.mem_assoc v vs -> of_value (List.assoc v vs)
 *   | EBinop (op,e1,e2) ->
 *      EInt 0  (\* TO FIX !!! *\)
 *      (\* begin match Mybuiltins.lookup_val op, subst vs e1, subst vs e2 with
 *       *   f, EInt c1, EInt c2 -> EInt (f [Val_int c1;Val_int c2])   Immediate reduction
 *       * | _, e1', e2' -> EBinop (op, e1', e2') 
 *       * end *\)
 *   (\* | ECond ((e11,op,e12),e2,e3) -> ECond ((subst vs e11,op,subst vs e12), subst vs e2, subst vs e3) *\)
 *   | ECond (e1,e2,e3) -> ECond (subst vs e1, subst vs e2, subst vs e3)
 *   | _ -> expr *)
               
(* Renaming *)

let rec rename f expr = match expr with
  (* Replace each variable [v] in [e] by [f v] *)
  | EVar v -> EVar (f v)
  | EBinop (op,e1,e2) -> EBinop (op, rename f e1, rename f e2)
  (* | ECond ((e11,op,e12),e2,e3) -> ECond ((rename f e11,op,rename f e12), rename f e2, rename f e3) *)
  | ECond (e1,e2,e3) -> ECond (rename f e1, rename f e2, rename f e3)
  | _ -> expr
       
(* Evaluation *)

(* exception Unknown_id of string
 * exception Unbound_id of string
 * exception Illegal_expr of t
 * 
 * let lookup env id = 
 *   try
 *     match List.assoc id env with
 *       Some v -> v
 *     | None -> raise (Unbound_id id)
 *   with 
 *     Not_found -> raise (Unknown_id id)
 * 
 * let rec eval env exp = 
 *   match exp with
 *     EInt v -> Val_int v
 *   | EBool v -> Val_bool v
 *   | EEnum c -> Val_enum c
 *   | EVar id -> lookup env id 
 *   | EBinop (op, exp1, exp2) ->
 *      Val_int 0 (\* TO FIX !! *\)
 *      (\* let f = Builtins.lookup_val op in
 *       * f [eval env exp1; eval env exp2] *\)
 *   | ECond (e1, e2, e3) ->
 *      (\* begin match eval_test exp env e1 with *\)
 *      begin match eval env e1 with
 *        Val_bool true -> eval env e2
 *      | Val_bool false -> eval env e3
 *      | _ -> raise (Illegal_expr exp)
 *      end
 * 
 * (\* and eval_test exp env (e1,op,e2) = 
 *  *      match Builtins.lookup Builtins.relops op, eval env e1, eval env e2 with
 *  *         f, Val_int v1, Val_int v2 -> Val_bool (f v1 v2)
 *  *       | _, _, _ -> raise (Illegal_expr exp) *\)
 * 
 * let rec eval_rel env exp = 
 *   match exp with
 *   | EBinop (op, exp1, exp2) ->
 *      true (\* TO FIX !!! *\)
 *       (\* begin match Builtins.lookup Builtins.relops op, eval env exp1, eval env exp2 with
 *        *   f, Val_int v1, Val_int v2 -> f v1 v2
 *        * | _ -> raise (Illegal_expr exp)
 *        * end *\)
 *   | _ -> raise (Illegal_expr exp) *)

(* Printing *)

let string_of_value v = match v with
  Val_int i -> string_of_int i
| Val_bool b -> string_of_bool b
| Val_enum s -> s

let string_of_opt_value = function
    None -> "?"
  | Some v -> string_of_value v

let string_of_op = function
    "mod" -> " mod "
  | op -> op

let rec to_string e = match e with
    EInt c -> string_of_int c
  | EBool b -> string_of_bool b
  | EEnum c -> c
  | EVar n -> n
  | EBinop (op,e1,e2) -> to_string e1 ^ string_of_op op ^ to_string e2 (* TODO : add parens *)
  (* | ECond (e1,e2,e3) -> string_of_test e1 ^ "?" ^ to_string e2 ^ ":" ^ to_string e3 (\* TODO : add parens *\) *)
  | ECond (e1,e2,e3) -> to_string e1 ^ "?" ^ to_string e2 ^ ":" ^ to_string e3 (* TODO : add parens *)

(* and string_of_test (e1,op,e2) = to_string e1 ^ string_of_op op ^ to_string e2 (\* TODO : add parens *\) *)
