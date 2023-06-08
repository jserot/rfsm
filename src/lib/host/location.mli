type t =
    Loc of string  (* Filename *)
         * int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)

val no_location: t
  
val get_current_location : unit -> t
val no_location : t
val input_name : string ref
val input_chan : in_channel ref
val input_lexbuf : Lexing.lexbuf ref

val pp_location: Format.formatter -> t -> unit
val pp_input_name: Format.formatter -> unit
val string_of_location: t -> string
