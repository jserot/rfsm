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
  | EBool of bool
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t

and value = 
  | Val_int of int
  | Val_bool of bool
  | Val_enum of string

and env = (string * value) list

module Builtins :
  sig
    val binops : (string * (int -> int -> int)) list
    val relops : (string * ('a -> 'a -> bool)) list
    exception Illegal_op of string
    val lookup : (string * 'a) list -> string -> 'a
  end

module VarSet : Set.S with type elt = string

val of_value : value -> t
val unset_event : 'a option
val set_event : value option

val vars_of : t -> VarSet.t
val subst : (string * value) list -> t -> t
val rename : (string -> string) -> t -> t

exception Unknown_id of string
exception Unbound_id of string
exception Illegal_expr of t

val lookup : (string * 'a option) list -> string -> 'a

val eval : (string * value option) list -> t -> value

val eval_rel : (string * value option) list -> t -> bool

(** {2 Printers} *)

val to_string : t -> string
val string_of_value : value -> string
val string_of_opt_value : value option -> string
