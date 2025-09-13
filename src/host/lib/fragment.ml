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

let from_json json = 
  let open Yojson.Basic.Util in
  let extract_iov j = member "id" j |> to_string, member "type" j |> to_string in
  let extract_iovs name j = j |> member name |> to_list |> List.map extract_iov in
  { inps = extract_iovs "inps" json;
    outps = extract_iovs "outps" json;
    vars =  extract_iovs "vars" json;
    obj = json |> member "obj" |> to_string; }

let from_string s =
  from_json (Yojson.Basic.from_string s)
