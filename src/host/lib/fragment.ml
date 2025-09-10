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
  jf_inps: (string * string) list; (* id, type expr *)
  jf_outps: (string * string) list; (* id, type expr *)
  jf_vars: (string * string) list; (* id, type expr *)
  jf_obj: string; (* fragment to analyse; ex ["guard x=1"]  *)
} [@@deriving show]

let from_json json = 
  let open Yojson.Basic.Util in
  let extract name j = j |> member name |> to_assoc |> List.map (fun (k,j) -> k, to_string j) in
  { jf_inps = extract "inps" json;
    jf_outps = extract "outps" json; 
    jf_vars =  extract "vars" json;
    jf_obj = json |> member "obj" |> to_string; }

let from_string s =
  from_json (Yojson.Basic.from_string s)
