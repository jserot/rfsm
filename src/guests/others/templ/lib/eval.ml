(* Template for defining the guest language dynamic semantics *)
(* The implementation defined in this file should match the signature [Guest.EVAL]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Value = Value

type env = Value.t Env.t

exception Illegal_expr of Syntax.expr

let mk_env () =  (* TO BE WRITTEN *)

let upd_env lhs v env = (* TO BE WRITTEN *)

let rec eval_expr env e = (* TO BE WRITTEN *)

let eval_bool env e = (* TO BE WRITTEN *)

let pp_env fmt env = (* TO BE WRITTEN *)
