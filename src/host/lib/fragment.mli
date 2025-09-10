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

(**{1 Program fragments (to be sent to the server) *)

type t = { 
  jf_inps: (string * string) list; (* id, type expr *)
  jf_outps: (string * string) list; (* id, type expr *)
  jf_vars: (string * string) list; (* id, type expr *)
  jf_obj: string; (* fragment to analyse; ex ["guard x=1"]  *)
  } [@@deriving show]

val from_json: Yojson.Basic.t -> t

val from_string: string -> t
