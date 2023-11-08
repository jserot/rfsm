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

exception Not_implemented of string
exception Fatal_error of string

type act_semantics =
  | Sequential
  | Synchronous
                           
let not_implemented m = raise (Not_implemented m)
let fatal_error m = raise (Fatal_error m)
let warning msg = Printf.printf "** Warning: %s\n" msg; flush stdout

