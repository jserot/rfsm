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
  | Version of string
  | Ok of string list
  | Error of string
  [@@deriving show]

let string_of_json j = 
  let no_nl c = if c = '\n' then ' ' else c in
  j |> Yojson.Basic.pretty_to_string  |> String.map no_nl

let to_string r = 
  let j = match r with
  | Version s ->
    `Assoc [("version", `String s)]
  | Ok fs ->
    `Assoc [("ok", `List (List.map (fun s -> `String s) fs))]
  | Error s ->
    `Assoc [("error", `String s)]
  in
  string_of_json j
