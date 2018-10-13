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
  | EBool of bool         
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t  (** e1 op e2 *)
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  | EFapp of string * t list  (** f(arg1,...,argn) *)
  | EArr of string * t        (** t[i] when t is an array *)
  | EBit of string * t        (** t[i] when t is an int *)
  | EBitrange of string * t * t   (** t[hi:lo] when t is an int *)
  | ECast of t * Type_expr.t

and e_val = 
  | Val_int of int
  | Val_float of float
  | Val_bool of bool
  | Val_enum of string
  | Val_fn of string list * t   (** args, body *)
  | Val_unknown                 
  | Val_none                    (** used for pure events *)
  | Val_array of e_val array

module VarSet : Set.S with type elt = string

exception Out_of_bound of string * int  (** array name, index value *)
                        
val array_update : string -> e_val array -> int -> e_val -> e_val array
  
val of_value : e_val -> e_desc

val type_of_value : e_val -> Types.typ
  (** [type_of_value v] returns the "best known" type for value [v].
      For an integer, this will always be [TyInt None].
      For an enumerated value [c], this will be the "approximation" [TyEnum [c]]. *)

val unset_event : e_val
val set_event : e_val

val vars_of : t -> VarSet.t
val rename : (string -> string) -> t -> t

(** {2 Printers} *)

val string_of_expr : e_desc -> string
val to_string : t -> string
val string_of_value : e_val -> string
val string_of_opt_value : e_val option -> string
