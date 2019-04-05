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

(** Static description of systems *)

module DepG : Graph.Sig.IM with type V.label = string and type E.label = string

type t = {
  m_name : string;
  m_models : Fsm.model list;
  m_fsms : Fsm.inst list;
  m_inputs : (string * global) list;
  m_outputs : (string * global) list;
  m_types: (string * Types.typ) list; 
  m_fns: (string * global) list; 
  m_consts: (string * global) list; 
  m_shared : (string * global) list;
  m_deps : dependencies;                (** dependency graph *)
}

and global = Types.typ * mg_desc

and mg_desc =
  | MInp of Global.stim_desc * string list     (** stimuli desc, reader(s) *)
  | MOutp of string list                 (** writer(s) *)
  | MFun of string list * Expr.t         (** args, body *)
  | MConst of Expr.value                 (** value *)
  | MShared of string list * string list (** writer(s), reader(s) *)

and dependencies = {
  md_graph : DepG.t;
  md_node : string -> DepG.V.t;
  }

(** {2 Exceptions} *)

exception Illegal_const_expr of Expr.t

(** {2 Builders} *)

val build : name:string
            -> ?gtyps:(string * Types.typ) list
            -> ?gfns:(string * global) list
            -> ?gcsts:(string * global) list
            -> Fsm.model list
            -> Fsm.inst list
            -> t
  (** [build name gtyps gfns gcsts models fsms] builds a system description from a list of global types, function
      and constant declarations, FSM models and instances *)

(** {2 Printers} *)
  
val dot_output :
  string ->
  ?dot_options:Utils.Dot.graph_style list ->
  ?fsm_options:Fsm.dot_options list ->
  t ->
  string list
  (** [dot_output dir m] writes a DOT representation of system [m] in directory [dir] and returns 
     a list of the written files *)

val dump : out_channel -> t -> unit
