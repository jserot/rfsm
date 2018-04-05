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

(** Builtin operations *)

type prim = Expr.e_val list -> Expr.e_val
          
type desc = Types.typ_scheme * prim  (** type, value *)
          
type env = (string * desc) list

val env: env

exception Unbound_id of string

val lookup: string -> desc
val lookup_typ: string -> Types.typ_scheme
val lookup_val: string -> prim
