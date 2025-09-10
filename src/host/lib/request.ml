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

type t =
    GetVersion
  | CheckFragment of Fragment.t
  [@@deriving show]

exception Invalid of string
    
let from_string s = 
  let json = Yojson.Basic.from_string s in
  let open Yojson.Basic.Util in
  match keys json with
  | ["version"] ->
      GetVersion
  | ["fragment"] ->
      let f = member "fragment" json in
      CheckFragment (Fragment.from_json f)
  | _ ->
    raise (Invalid s)
