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

let global_qualifier = "$" 
let print_scope = ref false (* For debug only *)

type scope = Local | Global
   (* [@@deriving show {with_path=false}] *)
                   
type t = {
    scope: scope;
    id: string
    }

exception Undefined of string * Location.t * t 
exception Duplicate of string * Location.t * t
                     
let mk ?(scope=Local) id = { scope=scope; id=id }

let mk_global id = { id with scope = Global }
let mk_local id = { id with scope = Local }

let upd_id f i = { i with id = f i.id }

let pp_qual fmt s =
  match s.scope with
  | Global -> Format.fprintf fmt "%s%s" global_qualifier s.id
  | Local -> Format.fprintf fmt "%s" s.id

let pp fmt i =
  if !print_scope then pp_qual fmt i else Format.fprintf fmt "%s" i.id

let to_string = Misc.to_string pp

let compare { id=id1; _ } { id=id2; _ } = Stdlib.compare id1 id2 (* Do not take scope into account *)
