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
  | Compile of string list
  | Close
  [@@deriving show]

exception Invalid of string
    
let from_string s = 
  let json = Yojson.Basic.from_string s in
  let open Yojson.Basic.Util in
  try 
    match keys json with
    | ["close"] ->
        Close
    | ["version"] ->
        GetVersion
    | ["check"] ->
        let f = member "check" json in
        CheckFragment (Fragment.from_json f)
    | ["compile"] ->
        let j = member "compile" json in
        let args = j |> member "args" |> to_list |> List.map to_string in
        Compile args
    | _ ->
      raise (Invalid s)
  with _ ->
    raise (Invalid s)
