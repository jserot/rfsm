(* The implementation defined in this file should match the signature [Guest.EVAL]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Value = Value

module Env = Rfsm.Env
module Annot = Rfsm.Annot

type env = Value.t Env.t

exception Illegal_expr of Syntax.expr
exception Uninitialized of Rfsm.Location.t

let mk_env () = Env.init []

let upd_env lhs v env = Env.upd lhs.Annot.desc v env

let lookup ~loc v env = 
 match Rfsm.Env.find v env with
  | Value.Val_unknown -> raise (Uninitialized loc)
  | v -> v

let eval_expr env e = match e.Annot.desc with
  | Syntax.EVar v -> lookup ~loc:e.Annot.loc v env
  | Syntax.EBool i -> Val_bool i 

let eval_bool env e = 
  match eval_expr env e with
  | Val_bool b -> b
  | _ -> raise (Illegal_expr e) (* Should not occur after TC *)

let pp_env fmt env = 
  Format.fprintf fmt "%a\n" (Env.pp ~sep:"=" Value.pp) env
