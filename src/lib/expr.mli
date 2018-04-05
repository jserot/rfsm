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
  | EBinop of string * t * t  (** e1 op e2 *)
  | ECond of t * t * t        (** e1 ? e2 : e3 *)
  (* | ECond of test * t * t        (\** e1 ? e2 : e3 *\) *)

(* and test = t * string * t     (\** e1 relop e2 *\) *)

type e_val = 
  | Val_int of int
  | Val_bool of bool
  | Val_enum of string

(* module Builtins :
 *   sig
 *     val binops : (string * (int -> int -> int)) list
 *     val relops : (string * ('a -> 'a -> bool)) list
 *     exception Illegal_op of string
 *     val lookup : (string * 'a) list -> string -> 'a
 *   end *)

module VarSet : Set.S with type elt = string

val of_value : e_val -> t
val unset_event : 'a option
val set_event : e_val option

val vars_of : t -> VarSet.t
(* val subst : (string * e_val) list -> t -> t *)
val rename : (string -> string) -> t -> t

(* exception Unknown_id of string
 * exception Unbound_id of string
 * exception Illegal_expr of t *)

(* val lookup : (string * 'a option) list -> string -> 'a
 * 
 * val eval : (string * e_val option) list -> t -> e_val
 * 
 * val eval_rel : (string * e_val option) list -> t -> bool *)

(** {2 Printers} *)

val to_string : t -> string
val string_of_value : e_val -> string
val string_of_opt_value : e_val option -> string
