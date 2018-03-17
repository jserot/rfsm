(** Interface to the simulator *)

exception Error of string

type config = { max_micro_reactions : int; }

val cfg : config

type stimulus = Ident.t * Expr.value option  (** name, value (None for pure events) *)
type response = Ident.t * Expr.value option   (** name, value (None for pure events) *)

type reaction = Types.date * string * Stimuli.stimuli list * response list * string

type context = {  (** The simulator state *)
  c_date: Types.date;
  c_inputs: (string * (Types.typ * Expr.value option)) list;   (** Global inputs *)
  c_outputs: (string * (Types.typ * Expr.value option)) list;  (** Globals outputs *)
  c_vars: (string * (Types.typ * Expr.value option)) list;     (** Shared variables *)
  c_evs: (string * (Types.typ * Expr.value option)) list;      (** Shared events *)
  c_fsms: Fsm.inst list * Fsm.inst list;                       (** FSMs, partitioned into active and inactive subsets *)
  }

(* val update_ctx : context -> Ident.t * Expr.value option -> context *)
exception OverReaction of Types.date
(* val global_updates : (Ident.t * 'a) list -> (string * 'a) list *)

val react : Types.date -> context -> stimulus list -> context * response list
  (** [react t ctx stimuli] computes a global reaction in [ctx] at time [t] given a set of stimuli [stimuli],
     producing an updated context [ctx'] and a set of responses [resps].
     The (operational) semantics is that of StateCharts (in turn similar to that of the delta concept
     used un DE formalisms. A reaction is viewed as a (finite) sequence of "micro-reactions".
     Each micro-reaction can generate stimuli which can trigger another micro-reaction (the related
     stimuli are here called "reentrant". However, a given FSM can only react once during a sequence of 
     micro-reactions (this is implemented by partitionning FSMs into active/inactive subsets during a reaction.
     A reaction ends when all the micro-reactions have taken place, i.e. when the last one did not produce
     any further re-entrant stimulus. *)

val run :
  ?ctx:context option ->
  Comp.t ->
  context list * (Types.date * (Ident.t * Expr.value option) list) list

(** {2 Printers} *)

(* val string_of_comp : string * ('a * Expr.value option) -> string
 * val string_of_fsm : Fsm.inst -> string *)
val dump_context : context -> unit
(* val string_of_event : Ident.t * Expr.value option -> string *)
val dump_reaction : int * (Ident.t * Expr.value option) list -> unit
