(** Reactive Finite State Machines *)

module State :
  sig
    type t = string
    val compare : t -> t -> int
    val to_string : t -> t
  end

module TransLabel :
  sig
    type t = Condition.t * Action.t list * bool  (* Boolean flag is true for implicit transitions *)
    val compare : t -> t -> int
    val to_string : t -> string
    val rename : (string -> string) -> t -> t 
      (** [rename f l] renames [f v] each variable [v] occurring in [l] *)
    val subst : Expr.env -> t -> t 
      (** [subst env l] replaces each variable [v] occuring in [l] by its value if found in [env],
          simplifying the resulting expression whenever possible. *)
  end

(** The internal representation, as a Labeled Transition System *)
     
module Repr : Lts.T with type state = State.t and type label = TransLabel.t 

type state = State.t
type transition = State.t * TransLabel.t * State.t
type itransition = TransLabel.t * State.t

(** Generic model *)
type model = {
  fm_name : string;                                      (** name *)
  fm_params : (string * Types.typ) list;                 (** generic parameters *)
  fm_inps : (string * Types.typ) list;                   (** inputs *)
  fm_outps : (string * Types.typ) list;                  (** outputs *)
  fm_vars : (string * Types.typ) list;                   (** internal variables *)
  fm_repr : Repr.t;                                      (** underlying LTS *)
  fm_resolve : (transition list -> transition) option;   (** resolution fonction for non-deterministic transitions *)
}

(** Model instances *)
type inst = {
  f_name : string;                                             (** name *)
  f_model : model;                                             (** bound model *)
  f_params : (string * (Types.typ * Expr.value)) list;         (** actual parameters *)
  f_inps : (string * (Types.typ * global)) list;               (** inputs, with bounded global *)
  f_outps : (string * (Types.typ * global)) list;              (** outputs, with bounded global *)
  f_vars : (string * (Types.typ * Expr.value option)) list;    (** internal variable, with value ([None] if not initialized) *)
  f_repr : Repr.t;                                             (** underlying LTS *)
  f_l2g : string -> string;                                    (** local to global name conversion function *)
  f_resolve : (transition list -> transition) option;          (** resolution fonction for non-deterministic transitions *)
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
| ValueChange of (int * Expr.value) list  (** (Date,value)s *)

(** {2 Accessors} *)

val states_of : inst -> state list
val istate_of : inst -> state option
val transitions_of : inst -> transition list
val itransitions_of : inst -> itransition list
val succs : inst -> state -> (state * TransLabel.t) list
val input_events_of : inst -> string list
val output_events_of : inst -> string list

(* val erase_type : 'a * ('b * 'c) -> 'a * 'c
 * val global_id : global -> string *)

(** {2 Static description} *)

(* val mk_bindings : local_names:'a list -> global_names:'a list -> 'a -> 'a *)

val build_model :
  name:string ->
  states:state list ->
  params:(string * Types.typ) list ->
  inps:(string * Types.typ) list ->
  outps:(string * Types.typ) list ->
  vars:(string * Types.typ) list ->
  trans:(state * (Condition.event * Condition.guard list) * Action.t list * state) list ->
  itrans:state * Action.t list ->
  model

val build_instance :
  name:string ->
  model:model ->
  params:(string * Expr.value) list ->
  inps:global list -> outps:global list ->
  inst

val sanity_check : inst -> unit

exception Undef_symbol of string * string * string
exception Internal_error of string
exception Invalid_state of string * string
exception Binding_mismatch of string * string * string
exception Invalid_parameter of string * string
exception Invalid_io of string * string * string
exception Type_mismatch of string * string * string * Types.typ * Types.typ


(** {2 Dynamic behavior} *)

type response = string * Expr.value option
(* val replace_assoc' :
 *   'a -> 'b -> ('a * ('c * 'b)) list -> ('a * ('c * 'b)) list
 * type fsm_env = (string * Expr.value option) list
 * val do_action :
 *   inst * (Ident.t * Expr.value option) list *
 *   (string * Expr.value option) list ->
 *   Action.t ->
 *   inst * (Ident.t * Expr.value option) list *
 *   (string * Expr.value option) list
 * val do_actions :
 *   (string * Expr.value option) list ->
 *   inst -> Action.t list -> inst * (Ident.t * Expr.value option) list
 * val mk_local_env :
 *   inst ->
 *   (string * Expr.value option) list -> (string * Expr.value option) list *)
val react :
  Types.date ->
  (string * Expr.value option) list ->
  inst -> inst * (Ident.t * Expr.value option) list

exception IllegalTrans of inst * string
exception Undeterminate of inst * string * Types.date
exception NonDetTrans of inst * transition list * Types.date

val fireable :
  inst ->
  (Condition.event * Expr.value option) list -> Repr.transition -> bool

val check_cond :
  inst -> (Condition.event * Expr.value option) list -> Condition.t -> bool

val is_event_set :
  (Condition.event * Expr.value option) list -> Condition.event -> bool

val init_fsm :
  (string * Expr.value option) list ->
  inst -> inst * (Ident.t * Expr.value option) list

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
