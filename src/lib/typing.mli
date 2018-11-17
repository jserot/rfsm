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

exception Unbound_id of string * string 
exception Typing_error of Expr.t * Types.typ * Types.typ 
exception Type_error of string * string * Types.typ * Types.typ (** what, where, type, type *)
exception Internal_error of string (** where *)
exception Illegal_cast of Expr.t
exception Unbound_type_ctor of string
exception Invalid_record_access of Expr.t

(** {2 Typing} *)

val type_of_type_expr : tenv -> Type_expr.t -> Types.typ
  (** [type_expression env te] returns the type represented by type expression [te] in environment [env] *)
  
val type_expression : tenv -> Expr.t -> Types.typ
  (** [type_expression env e] returns the type of expression [e] in environment [env], performing
      all required type checks. *)

val type_of_value : tenv -> Expr.e_val -> Types.typ
  (** [type_of_value env e] returns the best approximation of type of value [v] in environment [env] *)

(** {2 Printers} *)

val dump_tenv : out_channel -> tenv -> unit
