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
  | ScanFragment of Fragment.t
  | Compile of string list (* args, including anonymous one(s) *)
  | Close
  [@@deriving show]

exception Invalid of string

let of_yojson (json : Yojson.Basic.t) : t =
  let open Yojson.Basic.Util in
  match json |> member "kind" |> to_string with
  | "version" -> GetVersion
  | "close" -> Close
  | "check" ->
      let frag = Fragment.of_json (json |> member "fragment") in
      CheckFragment frag
  | "scan" ->
      let frag = Fragment.of_json (json |> member "fragment") in
      ScanFragment frag
  | "compile" ->
      let files = json |> member "args" |> to_list |> List.map to_string in
      Compile files
  | other ->
      
      raise (Invalid other)

let of_string s = 
  s |> Yojson.Basic.from_string |> of_yojson

let to_json (r : t) : Yojson.Basic.t =
  match r with
  | GetVersion ->
      `Assoc [("kind", `String "version")]
  | Close ->
      `Assoc [("kind", `String "close")]
  | CheckFragment frag ->
      `Assoc [
        ("kind", `String "check");
        ("fragment", Fragment.to_json frag)
      ]
  | ScanFragment frag ->
      `Assoc [
        ("kind", `String "scan");
        ("fragment", Fragment.to_json frag)
      ]
  | Compile files ->
      `Assoc [
        ("kind", `String "compile");
        ("args", `List (List.map (fun s -> `String s) files))
      ]

let to_string r =
  Yojson.Basic.pretty_to_string (to_json r)
