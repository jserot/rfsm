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

type t = {
    mutable e_desc: e_desc;
    mutable e_typ: Types.typ;
  }

and e_desc = 
    EInt of int
  | EFloat of float         
  | EChar of char         
  | EBool of bool
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  | EFapp of string * t list  (** f(arg1,...,argn) *)
  | EArrExt of t list         (** [e1,...,e2] *)
  | EArr of string * t        (** t[i] when t is an array *)
  | EBit of string * t        (** t[i] when t is an int *)
  | EBitrange of string * t * t   (** t[hi:lo] when t is an int *)
  | ERecord of string * string (** v.name when v is a record *)
  | ECast of t * Type_expr.t

let mk_expr e = { e_desc = e; e_typ = Types.no_type }
let mk_var v = mk_expr (EVar v)
              
type value = {
  mutable v_desc: e_val;
  mutable v_typ: Types.typ;
  }

and e_val = 
  | Val_int of int
  | Val_float of float
  | Val_char of char
  | Val_bool of bool
  | Val_enum of string
  | Val_fn of string list * t   (** args, body *)
  | Val_unknown
  | Val_none                    (** used for pure events *)
  | Val_array of value array
  | Val_record of (string * value) list  (** (Field name, value) list *)

let rec of_value v = match v.v_desc with
    Val_int v -> EInt v
  | Val_float f -> EFloat f
  | Val_char f -> EChar f
  | Val_bool b -> EBool b
  | Val_enum c -> EEnum c
  | Val_array vs -> EArrExt (List.map (fun v -> mk_expr (of_value v)) (Array.to_list vs))
  | _ -> Misc.fatal_error "Expr.of_value"

exception Out_of_bound of string * int

let mk_val ty v = { v_desc=v; v_typ=ty }

let mk_array vs =
  let ty = match vs with v::_ -> v.v_typ | _ -> Misc.fatal_error "Expr.mk_array" in
  { v_desc=Val_array (Array.of_list vs);
    v_typ=TyArray (TiConst (List.length vs), ty) }

let mk_record name fds = 
  { v_desc = Val_record (List.map (function (n,ty,v) -> n, v) fds);
    v_typ = TyRecord (name, List.map (function (n,ty,v) -> n,ty) fds); }

let mk_int v = mk_val (Types.type_int []) (Val_int v)
let mk_float v = mk_val Types.TyFloat (Val_float v)
let mk_char v = mk_val Types.TyChar (Val_char v)
let mk_bool v = mk_val Types.TyBool (Val_bool v)
             
let array_update id a i v =
  if i >= 0 && i < Array.length a
  then let a' = Array.copy a in Array.set a' i v; a'
  else raise (Out_of_bound (id,i))

let record_update id r f v = Utils.ListExt.replace_assoc f v r 

let unset_event = { v_desc=Val_bool false; v_typ=TyEvent }
let set_event = { v_desc=Val_bool true; v_typ=TyEvent }

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

let string_of_char c = "'" ^ String.make 1 c ^"'"

let rec string_of_val v = match v with
  Val_int i -> string_of_int i
| Val_float b -> string_of_float b
| Val_char c -> string_of_char c
| Val_bool b -> if b then "1" else "0"
| Val_enum s -> s
| Val_fn _ -> "<fun>"
| Val_unknown -> "<unknown>"
| Val_none -> "<none>"
| Val_array vs -> "[" ^ Utils.ListExt.to_string string_of_value "," (Array.to_list vs) ^ "]"
| Val_record r -> "{" ^ Utils.ListExt.to_string string_of_field_value "," r ^ "}"

and string_of_value v = string_of_val v.v_desc
                      
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
  | EChar c -> string_of_char c
  (* | EBool b -> string_of_bool b *)
  | EBool b -> if b then "1" else "0"
  | EEnum c -> c
  | EVar n -> n
  | EBinop (op,e1,e2) -> to_string e1 ^ string_of_op op ^ to_string e2 (* TODO : add parens *)
  | ECond (e1,e2,e3) -> to_string e1 ^ "?" ^ to_string e2 ^ ":" ^ to_string e3 (* TODO : add parens *)
  | EFapp (f,args) -> f ^ "(" ^ Utils.ListExt.to_string to_string "," args ^ ")"
  | EArrExt es -> "[" ^ Utils.ListExt.to_string to_string "," es ^ "]"
  | EArr (a,e') -> a ^ "[" ^ to_string e' ^ "]"
  | EBit (a,e') -> a ^ "[" ^ to_string e' ^ "]"
  | EBitrange (a,e1,e2) -> a ^ "[" ^ to_string e1 ^ ":" ^ to_string e2 ^ "]"
  | ERecord (a,f) -> a ^ "." ^ f
  | ECast (e,te) -> to_string e ^ "::" ^ Type_expr.string_of_type_expr te

and to_string e =
  let s = string_of_expr e.e_desc in
  s
