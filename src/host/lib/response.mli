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

(**{1 Server->client responses} *)

type t = 
  | Version of string
  | CompilationOk of string list (* list_of_generated_files *)
  | CompilationFailed of string (* error message *)
  | CheckingOk of string list * string list (* read vars, written vars *)
  | CheckingFailed of string (* error_message *)
  | Error of string
  | None
  [@@deriving show]
      
val to_string: t -> string
