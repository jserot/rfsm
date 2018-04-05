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

(** Model for Reactive Finite State Machines *)

(** States *)

module State :
  sig
    type t = string
    val compare : t -> t -> int
    val to_string : t -> t
  end

(** Transition labels *)

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
  end

(** The internal representation, as a Labeled Transition System *)
     
module Repr : Lts.T with type state = State.t and type label = TransLabel.t 

type state = State.t
type transition = State.t * TransLabel.t * State.t
type itransition = TransLabel.t * State.t

val string_of_transition: transition -> string
val string_of_state: state -> string
  
(** Abstract FSM model *)

type model = {
  fm_name : string;                                      (** name *)
  fm_params : (string * Types.typ) list;                 (** generic parameters *)
  fm_ios : (string * (Types.dir * Types.typ)) list;      (** i/os *)
  fm_vars : (string * Types.typ) list;                   (** internal variables *)
  fm_repr : Repr.t;                                      (** underlying LTS *)
}

(** Model for FSM instances *)
           
type inst = {
  f_name : string;                                             (** name *)
  f_model : model;                                             (** bound model *)
  f_params : (string * (Types.typ * Expr.e_val)) list;         (** actual parameters *)
  f_inps : (string * (Types.typ * global)) list;               (** inputs, with bounded global *)
  f_outps : (string * (Types.typ * global)) list;              (** outputs, with bounded global *)
  f_inouts : (string * (Types.typ * global)) list;             (** in/outs, with bounded global *)
  f_vars : (string * (Types.typ * Expr.e_val option)) list;    (** internal variable, with value ([None] if not initialized) *)
  f_repr : Repr.t;                                             (** underlying LTS *)
  f_l2g : string -> string;                                    (** local to global name conversion function *)
  f_state : string;                                            (** current state *)
  f_has_reacted: bool;                                         (** true when implied in the last reaction *)
}

(** Global IOs *)

and global =
  GInp of string * Types.typ * stim_desc       (** Global input, with type and description of associated stimuli *)
| GOutp of string * Types.typ                  (** Global output *)
| GShared of string * Types.typ                (** Shared variable or event *)

and stim_desc = 
  Periodic of int * int * int             (** Period, start time, end time *)
| Sporadic of int list                    (** Dates *)
| ValueChange of (int * Expr.e_val) list  (** (Date,value)s *)

(** {2 Accessors} *)

val states_of : inst -> state list
val istate_of : inst -> state option
val transitions_of : inst -> transition list
val itransitions_of : inst -> itransition list
val succs : inst -> state -> (state * TransLabel.t) list
val input_events_of : inst -> string list
val output_events_of : inst -> string list

(** {2 Static description} *)

val build_model :
  name:string ->
  states:state list ->
  params:(string * Types.typ) list ->
  ios:(Types.dir * string * Types.typ) list ->
  vars:(string * Types.typ) list ->
  trans:(state * (Condition.event * Condition.guard list) * Action.t list * state * int) list ->
  itrans:state * Action.t list ->
  model

val build_instance :
  name:string ->
  model:model ->
  params:(string * Expr.e_val) list ->
  ios:global list ->
  inst

val sanity_check : Typing.tenv -> inst -> unit

exception Undef_symbol of string * string * string (** FSM, kind, name *)
exception Internal_error of string (** where *)
exception Invalid_state of string * string (** FSM, id *)
exception Binding_mismatch of string * string * string  (** FSM, kind, id *)
exception Invalid_parameter of string * string (** FSM, name *)
exception Type_mismatch of string * string * string * Types.typ * Types.typ (** FSM, kind, id, type, type *)
exception Type_error of string * string * string * Types.typ * Types.typ (** FSM, what, id, type, type *)

(** {2 Dynamic behavior} *)

type response = string * Expr.e_val option

type act_semantics = Sequential | Synchronous 

val react :
  sem:act_semantics ->
  Types.date ->
  (string * Expr.e_val option) list ->
  inst -> inst * (Ident.t * Expr.e_val option) list

exception IllegalTrans of inst * string
exception Undeterminate of inst * string * Types.date
exception NonDetTrans of inst * transition list * Types.date

val fireable :
  inst ->
  (Condition.event * Expr.e_val option) list -> Repr.transition -> bool

val check_cond :
  inst -> (Condition.event * Expr.e_val option) list -> Condition.t -> bool

val is_event_set :
  (Condition.event * Expr.e_val option) list -> Condition.event -> bool

val init_fsm :
  sem:act_semantics ->
  (string * Expr.e_val option) list ->
  inst -> inst * (Ident.t * Expr.e_val option) list

(** {2 Printers} *)

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
  unit

val dot_output_model :
  ?fname:string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?options:dot_options list ->
  dir:string ->
  model ->
  unit

val dump_model : out_channel -> model -> unit

val dump_inst : out_channel -> inst -> unit
