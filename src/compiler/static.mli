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

(* Static elaboration *)

exception Unbound_fsm of Location.location * string
exception Unbound_global of Location.location * string
exception Fsm_mismatch of string * Location.location * string

type result = 
  Models of Fsm.model list
| System of Sysm.t
             
val elaborate: string -> Syntax.program -> result

val dot_output :
  string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?fsm_options:Fsm.dot_options list ->
  result ->
  unit

val dump: out_channel -> result -> unit                              
