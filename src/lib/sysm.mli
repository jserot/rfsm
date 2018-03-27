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

(** System description as a composition of FSM instances *)

module DepG : Graph.Sig.IM with type V.label = string and type E.label = string

type t = {
  m_name : string;
  m_fsms : Fsm.inst list;
  m_inputs : (string * global) list;
  m_outputs : (string * global) list;
  m_shared : (string * global) list;
  m_stimuli : Stimuli.stimuli list;
  m_deps : dependencies;                (** dependency graph *)
}

and global = Types.typ * mg_desc

and mg_desc =
  | MInp of istim_desc * string list     (** stimuli desc, reader(s) *)
  | MOutp of string list                 (** writer(s) *)
  | MShared of string list * string list (** writer(s), reader(s) *)

and istim_desc = {
  sd_comprehension : Fsm.stim_desc;
  sd_extension : Stimuli.event list;
}

and dependencies = {
  md_graph : DepG.t;
  md_node : string -> DepG.V.t;
  }

(** {2 Builders} *)

val build : name:string -> fsm_insts:Fsm.inst list -> t
  (** [build name fsms] builds a system description from a list of FSM instances *)

(** {2 Printers} *)
  
val dot_output :
  string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?fsm_options:Fsm.dot_options list ->
  ?with_insts:bool ->
  ?with_models:bool ->
  t ->
  unit

val dump : out_channel -> t -> unit
