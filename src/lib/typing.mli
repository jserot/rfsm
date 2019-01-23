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

(** Typing *)

type tenv =   (** Typing environment *)
  { te_vars: (string * Types.typ) list;
    te_ctors: (string * Types.typ) list;
    te_rfields: (string * Types.typ) list;
    te_defns: (string * Types.typ) list;
    te_prims: (string * Types.typ_scheme) list; }

val builtin_tenv: tenv

(** {2 Exceptions} *)

exception Undef_symbol of string * string * string (** where, what, name *)
exception Type_error of Expr.t * Types.typ * Types.typ 
exception Typing_error of string * string * Types.typ * Types.typ (** what, where, type, type *)
exception Illegal_cast of Expr.t
exception Unbound_type_ctor of string
exception Invalid_record_access of Expr.t

(** {2 Typing} *)

val type_of_type_expr : tenv -> Type_expr.t -> Types.typ
  (** [type_expression env te] returns the type represented by type expression [te] in environment [env] *)
  
val type_expression : tenv -> Expr.t -> Types.typ
  (** [type_expression env e] returns the type of expression [e] in environment [env], performing
      all required type checks. *)

val type_fsm_model : tenv -> Fsm.Static.model -> Types.typ
  (** [type_fsm_model env m] returns the type of FSM model [m] in environment [env], performing
      all required type checks. The type of a model [m] is [tp_1*...*tp_m -> ti_1*...*ti_n -> to_1*...*to_r]
      where [tp_k] is the type of the kth parameter, [ti_k] the type of kth input and [to_k] the type
      of the kth output. For a parameter-less model, the type is simply [ti_1*...*ti_n -> to_1*...*to_r] *)

val type_fsm_inst : tenv -> Fsm.Static.inst -> Types.typ
  (** [type_fsm_model env f] returns the type of FSM instance [f] in environment [env]. This function must be
      called after that parameters have been substituted by their values. The returned type is therefore sth like
      [ti_1*...*ti_n -> to_1*...*to_r] where  [ti_k] the type of kth input and [to_k] the type of the kth output. *)

val type_system : tenv -> Sysm.t -> unit
  (** [type_system m] typechecks system [m], raising exception [Typing_error] when needed. *)

(** {2 Printers} *)

val dump_tenv : out_channel -> tenv -> unit
