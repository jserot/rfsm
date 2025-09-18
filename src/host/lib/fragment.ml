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

type t = {
  inps: (string * string) list; (* id, type expr *)
  outps: (string * string) list; (* id, type expr *)
  vars: (string * string) list; (* id, type expr *)
  obj: string; (* fragment to analyse; ex ["guard x=1"]  *)
} [@@deriving show]

let of_json (json : Yojson.Basic.t) : t =
  let open Yojson.Basic.Util in
  { inps = json |> member "inps" |> to_list
           |> List.map (fun j -> (j |> member "id" |> to_string,
                                  j |> member "type" |> to_string));
    outps = json |> member "outps" |> to_list
           |> List.map (fun j -> (j |> member "id" |> to_string,
                                  j |> member "type" |> to_string));
    vars = json |> member "vars" |> to_list
           |> List.map (fun j -> (j |> member "id" |> to_string,
                                  j |> member "type" |> to_string));
    obj = json |> member "obj" |> to_string; }

(* let from_json json = 
 *   let open Yojson.Basic.Util in
 *   let extract_iov j = member "id" j |> to_string, member "type" j |> to_string in
 *   let extract_iovs name j = j |> member name |> to_list |> List.map extract_iov in
 *   { inps = extract_iovs "inps" json;
 *     outps = extract_iovs "outps" json;
 *     vars =  extract_iovs "vars" json;
 *     obj = json |> member "obj" |> to_string; } *)

let of_string s =
  of_json (Yojson.Basic.from_string s)

let to_json (f : t) : Yojson.Basic.t =
  let pair_list_to_yojson lst =
  `List (
    List.map (fun (a, b) ->
      `Assoc [("id", `String a); ("type", `String b)]
      ) lst
    ) in
  `Assoc [
    ("inps", pair_list_to_yojson f.inps);
    ("outps", pair_list_to_yojson f.outps);
    ("vars", pair_list_to_yojson f.vars);
    ("obj", `String f.obj)
  ]

let to_string f =
  Yojson.Basic.pretty_to_string (to_json f)
