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
exception Illegal_application of Expr.t
exception Illegal_array_access of Expr.t
exception Illegal_bit_range_access of Expr.t
exception Invalid_array_access of string * int (* array name, index value *)
exception Illegal_record_access of Expr.t
exception Non_static_expr of Expr.t * Expr.t

(** Evaluation environment *)

type env = (string * Expr.value) list

val subst : (string * Expr.value) list -> Expr.t -> Expr.t
  (** [subst senv e] substitutes each occurence of variable [v] listed in [senv] by its value in [e] *)

val eval : (string * Expr.value) list -> Expr.t -> Expr.value
  (** [eval env e] evaluates expression [e] in the context of environment [env] *)
