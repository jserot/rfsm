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

(** The simulator *)

exception Error of string

type config = {
  mutable max_micro_reactions: int;
  }

val cfg : config

type stimulus = Dynamic.loc * Expr.value
type response = Dynamic.loc * Expr.value 

type reaction = Types.date * string * Stimuli.stimuli list * response list * string

type context = {  (* The simulator state *)
  c_date: Types.date;
  c_inputs: (string * (Types.typ * Expr.value)) list;      (* Global inputs *)
  c_outputs: (string * (Types.typ * Expr.value)) list;     (* Global outputs *)
  c_fns: (string * (Types.typ * Expr.value)) list;         (* Global functions *)
  c_csts: (string * (Types.typ * Expr.value)) list;        (* Global constants *)
  c_vars: (string * (Types.typ * Expr.value)) list;        (* Shared variables *)
  c_evs: (string * (Types.typ * Expr.value)) list;         (* Shared events *)
  c_fsms: Dynamic.fsm list * Dynamic.fsm list;             (* FSMs, partitioned into active and inactive subsets *)
  }

exception OverReaction of Types.date

val react : Types.date -> context -> stimulus list -> context * response list
  (** [react t ctx stimuli] computes a global reaction in [ctx] at time [t] given a set of stimuli [stimuli],
     producing an updated context [ctx'] and a set of responses [resps].
     The (operational) semantics is that of StateCharts. A reaction is viewed as a (finite) sequence
     of "micro-reactions". Each micro-reaction can generate stimuli which can trigger another micro-reaction
     (the related stimuli are here called "reentrant". However, a given FSM can only react once during a sequence of 
     micro-reactions (this is implemented by partitionning FSMs into active/inactive subsets during a reaction.
     A reaction ends when all the micro-reactions have taken place, i.e. when the last one did not produce
     any further re-entrant stimulus. *)

val run : Static.t -> context * (Types.date * response list) list
  (** [run m] runs a simulation of system [m], returning the final context and a list of timed responses *)

(** {2 Printers} *)

val dump_context : context -> unit
val dump_reaction : int * Dynamic.event list -> unit
