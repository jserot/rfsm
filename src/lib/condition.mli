(** Trigerring condition for transitions *)

type t = event list * guard list
  (** The event list will be a singleton for normal transition, empty for initial transition *)

and event = string  (** event name *)

and guard = Expr.t * string * Expr.t  (** expr, rel_op, expr *)

val vars_of : t -> Expr.VarSet.t
val events_of : t -> Expr.VarSet.t

type env = (string * Expr.value option) list

val eval_guard : env -> guard -> bool

val eval_guards : env -> guard list -> bool
  (** [eval_guards env [g1;...gN]] is true iff [eval_guard env gi] is true for each i=1...N *)

val rename : (string -> string) -> t -> t
(** [rename f c] renames [f v] each variable [v] occurring in [c] *)

val subst : Expr.env -> t -> t
(** [subst env (evs,guards)] replaces each variable [v] occuring in [guards] by its value if found in [env],
   simplifying the resulting expression whenever possible. *)

(** {2 Printers} *)

val string_of_guard : Expr.t * string * Expr.t -> string
val to_string : t -> string
(* val to_string' : t -> string *)
