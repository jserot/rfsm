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

(** Trigerring condition for transitions *)

type t = event list * guard list
  (** The event list will be a singleton for normal transition, empty for initial transition *)

and event = string  (** event name *)

and guard = Expr.t

type env = (string * Expr.value) list

exception Illegal_guard_expr of Expr.t

(** {2 Accessors} *)

val vars_of : t -> Expr.VarSet.t
val events_of : t -> Expr.VarSet.t

(** {2 Operations} *)

val eval_guard : env -> guard -> bool

val eval_guards : env -> guard list -> bool
  (** [eval_guards env [g1;...gN]] is true iff [eval_guard env gi] is true for each i=1...N *)

val rename : (string -> string) -> t -> t
(** [rename f c] renames [f v] each variable [v] occurring in [c] *)

val subst : Eval.env -> t -> t
(** [subst env (evs,guards)] replaces each variable [v] occuring in [guards] by its value if found in [env],
   simplifying the resulting expression whenever possible. *)

(** {2 Printers} *)

val string_of_guard : guard -> string
val string_of_guards : guard list -> string
val to_string : t -> string
