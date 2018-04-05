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
    te_prims: (string * Types.typ_scheme) list; }

val builtin_tenv: tenv

(** {2 Exceptions} *)

exception Unbound_id of string * string 
exception Typing_error of Expr.t * Types.typ * Types.typ 

(** {2 Typing} *)
  
val type_expression : tenv -> Expr.t -> Types.typ
  (** [type_expression env e] returns the type of expression [e] in environment [env], performing
      all required type checks. *)

(** {2 Printers} *)

val dump_tenv : tenv -> unit
