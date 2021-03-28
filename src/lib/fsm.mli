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

(** {1 Reactive Finite State Machines} *)

open Lascar

(** Interpretation of actions associated to transitions *)
type act_semantics =
  | Sequential        (** sequential (ex: [x:=x+1;y:=x] with [x=1] gives [x=2,y=2]) *)
  | Synchronous       (** synchronous (ex: [x:=x+1:y=x] with [x=1] gives [x=2,y=1]) *)
  
type fsm_config = {
    mutable act_sep: string;         (** Default value: [" "] *)
    mutable act_sem: act_semantics;  (** Default value: [Sequential] *)
  }

val cfg: fsm_config

exception Undef_symbol of string * string * string (** FSM, kind, name *)

exception Invalid_state of string * string (** FSM, id *)

exception Typing_error of string * string * Types.typ * Types.typ (** what, where, type, type *)

exception Dubious_output_assignment of string * string * Action.t * string (** output name, state, action, where *)

exception Illegal_state_output of string * string * string (** FSM, state, name *)

(** {2 States} *)

module State :
  sig
    type t = string
    val compare : t -> t -> int
    val to_string : t -> t
  end

(** {2 State attributes} *)

module Attr :
sig
  type t = (string * Expr.t) list (** List of output valuations *)
  val to_string : t -> string
end
     
(** {2 Transition labels} *)

module TransLabel :
  sig
    type t = Condition.t * Action.t list * int * bool
      (** [(cond,acts,p,i)] means that the corresponding carrying transition will be
          taken whenever [cond] evaluates to [true], triggering actions [acts].
          [p] gives the priority level (used to resolve non-deterministic transitions).
          [i] indicates whether the corresponding transition is an implicit one. *)

    val compare : t -> t -> int
    val to_string : t -> string
    val rename : (string -> string) -> t -> t 
      (** [rename f l] renames [f v] each variable [v] occurring in [l] *)

    val subst : Eval.env -> t -> t 
      (** [subst env l] replaces each variable [v] occuring in [l] by its value if found in [env],
          simplifying the resulting expression whenever possible. *)

    val is_rtl : t -> bool
      (** [is_rtl t] returns [true] iff each variable written by transition [t] is written only once.
          For rtl transitions, the sequantial and synchronous interpretations are equivalent. *)
  end

(** {2 Internal representation} *)
     
module Repr : Ltsa.T with type state = State.t and type attr = Attr.t and type label = TransLabel.t 

type state = State.t
type attr = Attr.t
type transition = State.t * TransLabel.t * State.t
type itransition = TransLabel.t * State.t

val string_of_transition: transition -> string
val string_of_state: state -> string
val string_of_attr_state: state * attr -> string
  
(** {3 FSM model} *)

type model = {
    fm_name : string;                                      (** name *)
    fm_params : (string * Types.typ) list;                 (** generic parameters *)
    fm_ios : (string * (Types.dir * Types.typ)) list;      (** i/os *)
    fm_vars : (string * Types.typ) list;                   (** internal variables *)
    fm_repr : Repr.t;                                      (** underlying LTS *)
    (* mutable fm_typ : Types.typ; *)
  }

(** {3 FSM instance} *)
           
open Global
   
type inst = { 
    f_name: string;
    f_model: model;
    f_params: (string * (Types.typ * Expr.value)) list;       (** name, type, actual value *)
    f_inps: (string * (Types.typ * global)) list;             (** local name, (type, global) *)
    f_outps: (string * (Types.typ * global)) list;            (** local name, (type, global) *)
    f_inouts: (string * (Types.typ * global)) list;           (** local name, (type, global) *)
    f_vars: (string * Types.typ) list;                        (** name, type *)
    f_repr: Repr.t;                                           (** Static representation as a LTS (with _local_ names) *)
    f_l2g: string -> string;                                  (** local -> global name *)
  }

(** {2 Builders} *)

val build_model :
  name:string ->
  states:(state * attr) list ->
  params:(string * Types.typ) list ->
  ios:(Types.dir * string * Types.typ) list ->
  vars:(string * Types.typ) list ->
  trans:(state * (Condition.event * Condition.guard list) * Action.t list * state * int) list ->
  itrans:state * Action.t list ->
  model

val build_instance :
  name:string ->
  model:model ->
  params:(string * Expr.value) list ->
  ios:global list ->
  inst

(** {2 Transformers} *)

val normalize_model: model -> model
val normalize_inst: inst -> inst
(** Transform a Moore-style FSM model (resp. instance), with output values assigned to states,
    to a Mealy-style one, with all output values set on transitions *)

(** {2 Accessors} *)

val states_of_inst : inst -> state list
val attr_states_of_inst : inst -> (state * attr) list
val istate_of_inst : inst -> state option
val transitions_of_inst : inst -> transition list
val itransitions_of_inst : inst -> itransition list
val succs_inst : inst -> state -> (state * TransLabel.t) list
val input_events_of_inst : inst -> string list
val output_events_of_inst : inst -> string list
val is_rtl_inst : inst -> bool
  (** [is_rtl f] is [true] iff all [is_rtl a] for all actions [a] of [f] *)

val outputs_of_model : model -> (string * Types.typ) list
val inputs_of_model : model -> (string * Types.typ) list
val inouts_of_model : model -> (string * Types.typ) list
val states_of_model : model -> state list
val attr_states_of_model : model -> (state * attr) list
val istate_of_model : model -> state option
val transitions_of_model : model -> transition list
val itransitions_of_model : model -> itransition list
val succs_model : model -> state -> (state * TransLabel.t) list
val input_events_of_model : model -> string list
val output_events_of_model : model -> string list
val is_rtl_model : model -> bool
  (** [is_rtl f] is [true] iff all [is_rtl a] for all actions [a] of [f] *)

(** {2 Exceptions} *)

exception Binding_mismatch of string * string * string  (** FSM, kind, id *)
exception Invalid_parameter of string * string (** FSM, name *)
exception Uninstanciated_type_vars of string * string * string * string list (* FSM, kind, id, vars *)

(** {2 Externalizers} *)

type dot_options = OmitImplicitTransitions | GlobalNames | NoCaption

val dot_output_oc :
  out_channel ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?options:dot_options list ->
  inst ->
  unit

val dot_output :
  ?fname:string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?options:dot_options list ->
  dir:string ->
  inst ->
  string
  (** [dot_output dir f] writes a DOT representation of FSM instance [f] in directory [dir].
      Returns the name of the written file. *)

val dot_output_model :
  ?fname:string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?options:dot_options list ->
  dir:string ->
  model ->
  string
  (** [dot_output_model dir m] writes a DOT representation of FSM model [m] in directory [dir].
      Returns the name of the written file. *)

val dump_model : out_channel -> model -> unit
val dump_inst : out_channel -> inst -> unit
