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

(** Expressions and values *)

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
  | EBinop of string * t * t  (** e1 op e2 *)
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  | EFapp of string * t list  (** f(arg1,...,argn) *)
  | EArrExt of t list         (** [e1,...,e2] *)
  | EArr of string * t        (** t[i] when t is an array *)
  | EBit of string * t        (** t[i] when t is an int *)
  | EBitrange of string * t * t   (** t[hi:lo] when t is an int *)
  | ERecord of string * string (** v.name when v is a record *)
  | ECast of t * Type_expr.t

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


exception Out_of_bound of string * int  (** array name, index value *)
                        
(** {2 Builders} *)

val mk_expr : e_desc -> t
val mk_var : string -> t
  
val mk_val : Types.typ -> e_val -> value
val mk_array : value list -> value
val mk_record : Types.name -> (string * Types.typ * value) list -> value
val mk_int : int -> value
val mk_float : float -> value
val mk_char : char -> value
val mk_bool : bool -> value
val array_update : string -> value array -> int -> value -> value array
val record_update : string -> (string * value) list -> string -> value -> (string * value) list

val unset_event : value
val set_event : value
  
(** {2 Accessors} *)

val of_value : value -> e_desc

module VarSet : Set.S with type elt = string

val vars_of : t -> VarSet.t

(** {2 Manipulators} *)

val rename : (string -> string) -> t -> t

(** {2 Printers} *)

val string_of_expr : e_desc -> string
val to_string : t -> string
val string_of_val : e_val -> string
val string_of_value : value -> string
val string_of_opt_value : value option -> string
