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

(** Actions associated to transitions *)

type t = 
    Assign of lhs * Expr.t
  | Emit of string                            (** event *)
  | StateMove of string * string * string     (** fsm name, old state, new state *)

and lhs = { mutable l_desc: lhs_desc }

and lhs_desc = 
  | LhsVar of string                         (** v := ... *)
  | LhsArrInd of string * Expr.t             (** v[i] := ... when v is an array *)
  | LhsArrRange of string * Expr.t * Expr.t  (** v[hi:lo] := ... when v is an int *)
  | LhsRField of string * string             (** v.field_name when v has a record type *)

(** {2 Builders} *)

val mk_lhs : string -> lhs
  
(** {2 Accessors} *)

val lhs_name : lhs -> string
  
val vars_of : t -> Expr.VarSet.t * Expr.VarSet.t
  (** [vars_of a] returns the name of the variables read (resp. written) by action  [a] *)

(** {2 Operations} *)

val rename : (string -> string) -> t -> t
  (** [rename f a] renames [f v] each variable [v] occurring in [a] *)

val subst : Eval.env -> t -> t
  (** [subst env a] replaces each variable [v] occuring in [a] by its value if found in [env],
   simplifying the resulting expression whenever possible. *)

(** {2 Printers} *)

val string_of_lhs : lhs -> string
val to_string : t -> string
