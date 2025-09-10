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

type t = (* Server responses *)
  (* Request [GetVersion] *)
  | Version of string
  (* Request [CheckFragment] *)
  | NoErr
  | SyntaxErr
  | SemanticErr of string
  | TypingErr of string
  | OtherErr of string
  [@@deriving show]
      
let string_of_json j = 
  let no_nl c = if c = '\n' then ' ' else c in
  j |> Yojson.Basic.pretty_to_string  |> String.map no_nl

let to_string r = 
  let j = match r with
  | Version s ->
    `Assoc [("version", `String s)]
  | NoErr ->
    `Assoc [("result", `String "ok")]
  | SyntaxErr ->
    `Assoc [("result", `String ("syntax error"))]
  | OtherErr msg ->
    `Assoc [("result", `String ("error: " ^ msg))]
  | SemanticErr msg ->
    `Assoc [("result", `String ("semantic error: " ^ msg))]
  | TypingErr msg ->
    `Assoc [("result", `String ("typing error: " ^ msg))]
  in
  string_of_json j
