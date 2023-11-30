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

module Syntax = Syntax
module Value = Value

module Annot = Rfsm.Annot
module Env = Rfsm.Env
module Location = Rfsm.Location

open Value

type env = Value.t Env.t

exception Illegal_expr of Syntax.expr

let mk_env () = Env.empty 

let upd_env lval v env = 
  match lval.Annot.desc with
  | Syntax.LvalVar x -> Env.upd x v env

exception Uninitialized of Location.t

let lookup ~loc v env = 
 match Rfsm.Env.find v env with
  | Val_unknown -> raise (Uninitialized loc)
  | v -> v
  | exception Not_found -> raise (Rfsm.Misc.Fatal_error "Eval.lookup") (* Should not occur after TC *)

let rec eval_expr env e = match e.Annot.desc with
  | Syntax.EVar v -> lookup ~loc:e.Annot.loc v env
  | Syntax.EInt i -> Val_int i 
  | Syntax.EBool i -> Val_bool i 
  | Syntax.EBinop (op,e1,e2) -> 
     let f = Builtins.lookup op Builtins.eval_env in
     f [eval_arg env e1; eval_arg env e2]
  | Syntax.ECon0 c ->  Val_enum c.Rfsm.Ident.id

and eval_arg env e = match eval_expr env e with
    | Val_unknown -> raise (Uninitialized e.Annot.loc)
    | v -> v

let eval_bool env e = 
  match eval_expr env e with
  | Val_bool b -> b
  | _ -> raise (Illegal_expr e) (* Should not occur after TC *)

let pp_env fmt env = 
  Format.fprintf fmt "%a\n" (Env.pp ~sep:"=" Value.pp) env
