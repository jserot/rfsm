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

(** Dynamic models (used by the simulator) *)

(** {3 FSM} *)

type fsm = { 
    f_static: Fsm.inst;                                       (** Static representation *)
    f_vars: (string * (Types.typ * Expr.value)) list;         (** name, (type, value) *)
    f_state: string;                                          (** Current state *)
    f_has_reacted: bool;                                      (** true when implied in the last reaction *)
  }
          
(** {3 Local evaluation environment} *)
       
type lenv = (string * Expr.value) list

(** {3 Global evaluation environment} *)

type genv = {
    fe_inputs: (string * (Types.typ * Expr.value)) list;   (** Global inputs *)
    fe_csts: (string * (Types.typ * Expr.value)) list;     (** Global constants *)
    fe_fns: (string * (Types.typ * Expr.value)) list;      (** Global functions *)
    fe_vars: (string * (Types.typ * Expr.value)) list;     (** Shared variables *)
    fe_evs: (string * (Types.typ * Expr.value)) list;      (** Shared events *)
  }

(** {2 Builders} *)

val make_fsm: Fsm.inst -> fsm
  (** [create sf] creates a dynamic FSM instance from a static description [sf] *)
  
(** Input and output events *)

type event = loc * Expr.value  (** Event location, new value *)

and loc =
  | LVar of Ident.t             (** Scalar *)
  | LArrInd of Ident.t * int    (** 1D array location *)
  | LRField of Ident.t * string (** Record field *)

(** {2 Exceptions} *)

exception IllegalTrans of fsm * string
exception IllegalAction of fsm * Action.t
exception Undeterminate of fsm * string * Types.date
exception NonDetTrans of fsm * Fsm.transition list * Types.date
exception NonAtomicIoWrite of fsm * Action.t

(** {2 Accessors} *)

val fireable : fsm -> lenv -> Fsm.Repr.transition -> bool
  (** [fireable f env t] returns [true] iff transition [t] in FSM [f] is fireable, given ocal env [env]. *)
  
val check_cond : fsm -> lenv -> Condition.t -> bool
  (** [check_cond f env c] evaluatues condition [c] in FSM [f], in the context of local env [env]. *)

val is_event_set : lenv -> Condition.event -> bool
  (** [is_event_set env ev] indicates whether event [ev] is set in local environment [env] *)

(** {2 Simulation} *)

val init: genv -> fsm -> fsm * event list
  (** [init env f] performs the initial transition of FSM [f], in global environment [env],
      Returns an updated fsm and list of events consisting of
     - updates to global outputs or shared objects
     - updates to local variables (including state move) *)

val react: Types.date -> genv -> fsm -> fsm * event list
  (** [react t env f] compute the reaction, at time [t] of FSM [f] in global environment [env].
     The global environment contains the values of global inputs, shared objects and global functions/constants.
     As for [init], returns an updated fsm and list of events. *)
