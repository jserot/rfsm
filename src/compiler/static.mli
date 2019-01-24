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

type program = Sysm.t
             
val elaborate: string -> Syntax.program -> program

val dot_output :
  string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?fsm_options:Fsm.Static.dot_options list ->
  ?with_insts:bool ->
  ?with_models:bool ->
  program ->
  unit

val dump: out_channel -> program -> unit                              
