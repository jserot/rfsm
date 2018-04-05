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

type env = (string * Expr.e_val) list

open Expr

exception Unknown_id of string
exception Unbound_id of string
exception Illegal_expr of Expr.t
   
let rec subst vs expr = match expr with
  (* Substitute each occurence of variables in [vs] by its value in [expr] *)
  | EVar v when List.mem_assoc v vs -> of_value (List.assoc v vs)
  | EBinop (op,e1,e2) ->
     begin match Builtns.lookup_val op, subst vs e1, subst vs e2 with
       f, EInt c1, EInt c2 ->   (* Immediate reduction *)
        begin match f [Val_int c1;Val_int c2] with
          Val_int v -> EInt v
        | _ -> raise (Illegal_expr expr)
        end
     | _, e1', e2' -> EBinop (op, e1', e2') 
     end
  (* | ECond ((e11,op,e12),e2,e3) -> ECond ((subst vs e11,op,subst vs e12), subst vs e2, subst vs e3) *)
  | ECond (e1,e2,e3) -> ECond (subst vs e1, subst vs e2, subst vs e3)
  | _ -> expr
               
let lookup env id = 
  try
    match List.assoc id env with
      Some v -> v
    | None -> raise (Unbound_id id)
  with 
    Not_found -> raise (Unknown_id id)

let rec eval env exp = 
  match exp with
    EInt v -> Val_int v
  | EBool v -> Val_bool v
  | EEnum c -> Val_enum c
  | EVar id -> lookup env id 
  | EBinop (op, exp1, exp2) ->
     let f = Builtns.lookup_val op in
     f [eval env exp1; eval env exp2]
  | ECond (e1, e2, e3) ->
     (* begin match eval_test exp env e1 with *)
     begin match eval env e1 with
       Val_bool true -> eval env e2
     | Val_bool false -> eval env e3
     | _ -> raise (Illegal_expr exp)
     end

(* and eval_test exp env (e1,op,e2) = 
 *      match Builtns.lookup Builtns.relops op, eval env e1, eval env e2 with
 *         f, Val_int v1, Val_int v2 -> Val_bool (f v1 v2)
 *       | _, _, _ -> raise (Illegal_expr exp) *)

(* let rec eval_rel env exp = 
 *   match exp with
 *   | EBinop (op, exp1, exp2) ->
 *       begin match Builtns.lookup Builtns.relops op, eval env exp1, eval env exp2 with
 *         f, Val_int v1, Val_int v2 -> f v1 v2
 *       | _ -> raise (Illegal_expr exp)
 *       end
 *   | _ -> raise (Illegal_expr exp) *)
