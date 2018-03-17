(** Expressions and values *)

type t =
    EConst of int
  | EEnum of string
  | EVar of string
  | EBinop of string * t * t

and value = Val_enum of string | Val_int of int

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

(* val subst_vars : (string * value) list -> t -> t *)

(* val string_of_value : value -> string
 * val string_of_op : string -> string *)

(** {2 Printers} *)

val to_string : t -> string
val string_of_value : value -> string
val string_of_opt_value : value option -> string
