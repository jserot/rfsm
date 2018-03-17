(** System description as a composition of FSMs *)

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

(* exception Unknown_global of string *)

(* val build_stim : Fsm.stim_desc -> istim_desc
 * 
 * val extract_globals :
 *   (string * (Types.typ * mg_desc)) list *
 *   (string * (Types.typ * mg_desc)) list *
 *   (string * (Types.typ * mg_desc)) list ->
 *   Fsm.inst ->
 *   (string * (Types.typ * mg_desc)) list *
 *   (string * (Types.typ * mg_desc)) list *
 *   (string * (Types.typ * mg_desc)) list
 * val build_dependencies :
 *   Fsm.inst list -> (DepG.E.label * ('a * mg_desc)) list -> dependencies *)

val build_composite : name:string -> fsm_insts:Fsm.inst list -> t
  (** [build_composite name fsms] builds a system description from a list of FSM instances *)

(** {2 Printers} *)
  
(* val string_of_shared : string * (Types.typ * mg_desc) -> string *)

val dot_output :
  string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?fsm_options:Fsm.dot_options list ->
  ?with_insts:bool ->
  ?with_models:bool ->
  t ->
  unit

(* val dump_global : out_channel -> string * (Types.typ * mg_desc) -> unit
 * val dump_stimuli :
 *   out_channel -> int * (Ident.t * Expr.value option) list -> unit
 * val dump_dependencies : t -> unit *)
val dump : out_channel -> t -> unit
