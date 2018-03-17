(** Types *)

type date = int

type typ =
    TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option

and int_range = type_index * type_index (* min, max *)

and type_index =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index * type_index

exception Illegal_type_index of string * Expr.value 
exception Unbound_type_index of string

val is_event_type : typ -> bool

val vars_of : typ -> string list

type env = (string * Expr.value) list
         
val subst : env -> typ -> typ
  
val type_equal : env -> typ -> typ -> bool

(** {2 Printers} *)

val string_of_range : type_index * type_index -> string

val string_of_type : typ -> string
