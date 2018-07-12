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

(** Evaluating and manipulating expressions *)

exception Unknown_id of string
exception Illegal_expr of Expr.t

type env = (string * Expr.e_val) list

val subst : (string * Expr.e_val) list -> Expr.t -> Expr.t

val eval : (string * Expr.e_val) list -> Expr.t -> Expr.e_val
