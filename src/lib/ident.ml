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
  Local of string * string
| Global of string

let to_string = function
  | Local (l,i) -> l ^ "." ^ i
  | Global i -> i

let local_of = function
  | Local (l,_) -> l
  | Global _ -> ""
