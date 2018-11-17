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

open Utils

type t = {
    mutable e_desc: e_desc;
    mutable e_typ: Types.typ;
  }

and e_desc = 
    EInt of int
  | EFloat of float         
  | EBool of bool
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  | EFapp of string * t list  (** f(arg1,...,argn) *)
  | EArr of string * t        (** t[i] when t is an array *)
  | EBit of string * t        (** t[i] when t is an int *)
  | EBitrange of string * t * t   (** t[hi:lo] when t is an int *)
  | ERecord of string * string (** v.name when v is a record *)
  | ECast of t * Type_expr.t

and e_val = 
  | Val_int of int
  | Val_float of float
  | Val_bool of bool
  | Val_enum of enum_value
  | Val_fn of string list * t   (** args, body *)
  | Val_unknown
  | Val_none                    (** used for pure events *)
  | Val_array of e_val array
  | Val_record of record_value

and enum_value = {
    ev_typ: string; (** Type name *)
    ev_val: string;
  }

and record_value = {
    rv_typ: string; (** Record type name *)
    rv_val: (string * e_val) list  (** (Field name, value) list *)
  }

let of_value = function
    Val_int v -> EInt v
  | Val_float f -> EFloat f
  | Val_bool b -> EBool b
  | Val_enum c -> EEnum c.ev_val
  | Val_fn _ -> failwith "Expr.of_value"
  | Val_unknown -> failwith "Expr.of_value"
  | Val_none -> failwith "Expr.of_value"
  | _ -> failwith "Expr.of_value"

exception Out_of_bound of string * int
                        
let array_update id a i v =
  if i >= 0 && i < Array.length a
  then let a' = Array.copy a in Array.set a' i v; a'
  else raise (Out_of_bound (id,i))

let record_update id r f v = ListExt.replace_assoc f v r 

let unset_event = Val_bool false
let set_event = Val_bool true

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
let rec vars_of expr =
  match expr.e_desc with
    EVar v -> VarSet.singleton v
  | EBinop (_,e1,e2) -> VarSet.union (vars_of e1) (vars_of e2)
  | ECond (e1,e2,e3) -> List.fold_left (fun acc e -> VarSet.union acc (vars_of e)) VarSet.empty [e1;e2;e3]
  | EFapp (f,es) -> List.fold_left (fun acc e -> VarSet.union acc (vars_of e)) VarSet.empty es
  | EArr (a,e') -> vars_of e'
  | _ -> VarSet.empty
       
(* Renaming *)

let rec rename f expr = match expr.e_desc with
  (* Replace each variable [v] in [e] by [f v] *)
  | EVar v -> { expr with e_desc=EVar (f v) }
  | EBinop (op,e1,e2) -> { expr with e_desc=EBinop (op, rename f e1, rename f e2) }
  | ECond (e1,e2,e3) -> { expr with e_desc=ECond (rename f e1, rename f e2, rename f e3) }
  | EFapp (fn, es) -> { expr with e_desc=EFapp (fn, List.map (rename f) es) }
  | EArr (a, e') -> { expr with e_desc=EArr (a, rename f e') }
  | _ -> expr
       
(* Printing *)

let rec string_of_value v = match v with
  Val_int i -> string_of_int i
| Val_float b -> string_of_float b
| Val_bool b -> if b then "1" else "0"
| Val_enum s -> s.ev_val
| Val_fn _ -> "<fun>"
| Val_unknown -> "<unknown>"
| Val_none -> "<none>"
| Val_array vs -> "[" ^ ListExt.to_string string_of_value "," (Array.to_list vs) ^ "]"
| Val_record r -> "{" ^ ListExt.to_string string_of_field_value "," r.rv_val ^ "}"

and string_of_field_value (n,v) = n ^ "=" ^ string_of_value v

let string_of_opt_value = function
    None -> "?"
  | Some v -> string_of_value v

let string_of_op = function
    "mod" -> " mod "
  | op -> op

let rec string_of_expr e = match e with
    EInt c -> string_of_int c
  | EFloat b -> string_of_float b
  (* | EBool b -> string_of_bool b *)
  | EBool b -> if b then "1" else "0"
  | EEnum c -> c
  | EVar n -> n
  | EBinop (op,e1,e2) -> to_string e1 ^ string_of_op op ^ to_string e2 (* TODO : add parens *)
  | ECond (e1,e2,e3) -> to_string e1 ^ "?" ^ to_string e2 ^ ":" ^ to_string e3 (* TODO : add parens *)
  | EFapp (f,args) -> f ^ "(" ^ ListExt.to_string to_string "," args ^ ")"
  | EArr (a,e') -> a ^ "[" ^ to_string e' ^ "]"
  | EBit (a,e') -> a ^ "[" ^ to_string e' ^ "]"
  | EBitrange (a,e1,e2) -> a ^ "[" ^ to_string e1 ^ ":" ^ to_string e2 ^ "]"
  | ERecord (a,f) -> a ^ "." ^ f
  | ECast (e,te) -> to_string e ^ "::" ^ Type_expr.string_of_type_expr te

and to_string e =
  let s = string_of_expr e.e_desc in
  (* "(" ^ s ^ ":" ^ Types.string_of_type e.e_typ ^ ")" *)
  s
