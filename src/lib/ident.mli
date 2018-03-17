(** Identifiers *)

type t =
  | Local of string * string
  | Global of string

val to_string : t -> string
val local_of : t -> string
