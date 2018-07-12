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

type t = 
    EInt of int
  | EFloat of float         
  | EBool of bool         
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t  (** e1 op e2 *)
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  | EFapp of string * t list  (** f(arg1,...,argn) *)

and e_val = 
  | Val_int of int
  | Val_float of float
  | Val_bool of bool
  | Val_enum of string
  | Val_fn of string list * t   (** args, body *)
  | Val_unknown                 
  | Val_none                    (** used for pure events *)

module VarSet : Set.S with type elt = string

val of_value : e_val -> t

val unset_event : e_val
val set_event : e_val

val vars_of : t -> VarSet.t
val rename : (string -> string) -> t -> t

(** {2 Printers} *)

val to_string : t -> string
val string_of_value : e_val -> string
val string_of_opt_value : e_val option -> string
