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
  | EFloat of float         
  | EBool of bool
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t
  | ECond of t * t * t        (** e1 ? e2 : e3 *)

type e_val = 
  | Val_int of int
  | Val_float of float
  | Val_bool of bool
  | Val_enum of string

let of_value = function
    Val_int v -> EInt v
  | Val_float f -> EFloat f
  | Val_bool b -> EBool b
  | Val_enum c -> EEnum c

let unset_event = None
let set_event = Some (Val_int 1)

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
let rec vars_of expr =
  match expr with
    EVar v -> VarSet.singleton v
  | EBinop (_,e1,e2) -> VarSet.union (vars_of e1) (vars_of e2)
  | ECond (e1,e2,e3) ->
     List.fold_left (fun acc e -> VarSet.union acc (vars_of e)) VarSet.empty [e1;e2;e3]
  | _ -> VarSet.empty
       
(* Renaming *)

let rec rename f expr = match expr with
  (* Replace each variable [v] in [e] by [f v] *)
  | EVar v -> EVar (f v)
  | EBinop (op,e1,e2) -> EBinop (op, rename f e1, rename f e2)
  (* | ECond ((e11,op,e12),e2,e3) -> ECond ((rename f e11,op,rename f e12), rename f e2, rename f e3) *)
  | ECond (e1,e2,e3) -> ECond (rename f e1, rename f e2, rename f e3)
  | _ -> expr
       
(* Printing *)

let string_of_value v = match v with
  Val_int i -> string_of_int i
| Val_float b -> string_of_float b
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
  | EFloat b -> string_of_float b
  | EBool b -> string_of_bool b
  | EEnum c -> c
  | EVar n -> n
  | EBinop (op,e1,e2) -> to_string e1 ^ string_of_op op ^ to_string e2 (* TODO : add parens *)
  | ECond (e1,e2,e3) -> to_string e1 ^ "?" ^ to_string e2 ^ ":" ^ to_string e3 (* TODO : add parens *)
