type location =
    Loc of string  (* Filename *)
         * int     (* Position of the first character *)
         * int     (* Position of the next character following the last one *)

val get_current_location : unit -> location
val no_location : location
val input_name : string ref
val input_chan : in_channel ref
val input_lexbuf : Lexing.lexbuf ref

val output_location: out_channel -> location -> unit
val output_input_name: out_channel -> unit
val string_of_location: location -> string
