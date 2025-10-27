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

(**{1 Client->server requests} *)

type t =
    GetVersion
  | CheckFragment of Fragment.t
  | ScanFragment of Fragment.t
  | Compile of string list (* arguments, as on command line *)
  | Close
  [@@deriving show]

exception Invalid of string

val of_yojson: Yojson.Basic.t -> t
val of_string: string -> t
val to_json: t -> Yojson.Basic.t
val to_string: t -> string
