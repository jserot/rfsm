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

(**{1 Source program locations} *)

type t =
  Loc of string * int * int
     (** Filename, position of the first character, position of the next character following the last one *)

val mk: Lexing.position * Lexing.position -> t 

val get_current_location : unit -> t
val no_location : t
val input_name : string ref
val input_chan : in_channel ref
val input_lexbuf : Lexing.lexbuf ref

val pp_location: Format.formatter -> t -> unit
val pp_input_name: Format.formatter -> unit
val string_of_location: t -> string
val text_of_location: t -> string
