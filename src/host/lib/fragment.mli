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

(** {1 Program fragments (to be checked in server mode) *)

type t = { 
  inps: (string * string) list; (* id, type expr *)
  outps: (string * string) list; (* id, type expr *)
  vars: (string * string) list; (* id, type expr *)
  obj: string; (* fragment to analyse; ex ["guard x=1"]  *)
  } [@@deriving show]

val of_json: Yojson.Basic.t -> t
val of_string: string -> t
val to_json: t -> Yojson.Basic.t
val to_string: t -> string
