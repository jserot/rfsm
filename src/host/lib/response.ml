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
  | CompilationOk of string list (* list_of_generated_files *)
  | CompilationFailed of string (* error message *)
  | CheckingOk of string list * string list (* read vars, written vars *)
  | CheckingFailed of string (* error_message *)
  | Error of string
  | None
  [@@deriving show]

let string_of_json j = 
  let no_nl c = if c = '\n' then ' ' else c in
  j |> Yojson.Basic.pretty_to_string  |> String.map no_nl

let to_json (r: t) : Yojson.Basic.t =
  match r with
  | Version s ->
      `Assoc [("kind", `String "version"); ("version", `String s)]
  | CompilationOk files ->
      `Assoc [
        ("kind", `String "compiled");
        ("result", `Bool true);
        ("files", `List (List.map (fun s -> `String s) files))
      ]
  | CompilationFailed msg ->
      `Assoc [
        ("kind", `String "compiled");
        ("result", `Bool false);
        ("message", `String msg)
      ]
  | CheckingOk (rds,wrs) ->
      `Assoc [
        ("kind", `String "checked");
        ("result", `Bool true);
        ("rds", `List (List.map (fun v -> `String v) rds));
        ("wrs", `List (List.map (fun v -> `String v) wrs));
      ]
  | CheckingFailed msg ->
      `Assoc [
        ("kind", `String "checked");
        ("result", `Bool false);
        ("message", `String msg)
      ]
  | Error msg ->
      `Assoc [("kind", `String "error"); ("message", `String msg)]
  | None ->
      `Assoc [("kind", `String "none")]

let to_string r =
  Yojson.Basic.pretty_to_string (to_json r)

(* let to_string r = 
 *   let j = match r with
 *   | Version s ->
 *     `Assoc [("version", `String s)]
 *   | Compiled (true,fs) ->
 *     `Assoc [("result",`String "ok"); ("files", `List (List.map (fun s -> `String s) fs))]
 *   | Compiled (false,[msg]) ->
 *     `Assoc [("result",`String "failed"); ("error",`String msg)]
 *   | Compiled (false,_) ->
 *     `Assoc [("error", `String "Illegal response")] (\* should not happen *\)
 *   | Checked (true,_) -> (\* TBC *\)
 *     `Assoc [("result",`String "ok")]
 *   | Checked (false,msg) ->
 *     `Assoc [("result",`String "failed"); ("error",`String msg)]
 *   | None ->
 *     `String "<none>" (\* Should not happen *\)
 *   | Error s ->
 *     `Assoc [("error", `String s)]
 *   in
 *   string_of_json j *)

exception Invalid of string
    
let of_json (json : Yojson.Basic.t) : t =
  let open Yojson.Basic.Util in
  match json |> member "kind" |> to_string with
  | "version" ->
      Version (json |> member "version" |> to_string)
  | "compiled" ->
      let res = json |> member "result" |> to_bool in
      if res then
        let files = json |> member "files" |> to_list |> List.map to_string in
        CompilationOk files
      else
        let msg = json |> member "message" |> to_string in
        CompilationFailed msg
  | "checked" ->
      let res = json |> member "result" |> to_bool in
      if res then
        let rds = json |> member "rds" |> to_list |> List.map to_string in
        let wrs = json |> member "wrs" |> to_list |> List.map to_string in
        CheckingOk (rds,wrs)
      else
        let msg = json |> member "message" |> to_string in
        CheckingFailed msg
  | "error" ->
      Error (json |> member "message" |> to_string)
  | "none" ->
      None
  | other ->
      raise (Invalid other)
