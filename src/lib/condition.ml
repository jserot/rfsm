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

type t = event list * guard list

and event = string 

and guard = Expr.t * string * Expr.t

let vars_of (evs, gs) =
  let add_vars acc (exp,op,exp') = acc |> Expr.VarSet.union (Expr.vars_of exp) |>  Expr.VarSet.union (Expr.vars_of exp') in
  List.fold_left add_vars (Expr.VarSet.of_list evs) gs
  
let events_of (evs, gs) = Expr.VarSet.of_list evs
                  
let string_of_guard (exp,op,exp') = "(" ^ Expr.to_string exp ^ op ^ Expr.to_string exp' ^ ")"

let string_of_guards cs = match cs with
  [] -> ""
|  _ -> "." ^ Utils.ListExt.to_string string_of_guard "." cs

let to_string' (evs,gs) = Utils.ListExt.to_string Utils.Misc.id "." evs ^ string_of_guards gs 

let to_string c = to_string' c

type env = (string * Expr.value option) list

let eval_guard env (exp,op,exp') = (Expr.Builtins.lookup Expr.Builtins.relops op) (Expr.eval env exp) (Expr.eval env exp')

let eval_guards env gs = List.for_all (eval_guard env) gs (* Conjonctive semantics *)

let rename f (evs,gs) = (List.map f evs, List.map (function (e1,op,e2) -> Expr.rename f e1, op, Expr.rename f e2) gs)

let subst env (evs,gs) =
  let subst_guard (e,op,e') = Expr.subst env e, op, Expr.subst env e' in
  evs, List.map subst_guard gs
