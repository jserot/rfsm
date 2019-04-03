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

and guard = Expr.t

let vars_of (evs, gs) =
  let add_vars acc gexp = acc |> Expr.VarSet.union (Expr.vars_of gexp) in
  List.fold_left add_vars (Expr.VarSet.of_list evs) gs
  
let events_of (evs, gs) = Expr.VarSet.of_list evs
                  
let string_of_guard exp = Expr.to_string exp

let string_of_guards cs = match cs with
  [] -> ""
| [c] -> string_of_guard c
|  _ -> Utils.ListExt.to_string (fun c -> "(" ^ string_of_guard c ^ ")") "." cs

let to_string' (evs,gs) = Utils.ListExt.to_string Utils.Misc.id "." evs ^ "." ^  string_of_guards gs 

let to_string c = to_string' c

type env = (string * Expr.value) list

exception Illegal_guard_expr of Expr.t
                              
let eval_guard env exp =
  match Eval.eval env exp with
    { v_desc=Val_bool b } -> b
  | _ -> raise (Illegal_guard_expr exp)

let eval_guards env gs = List.for_all (eval_guard env) gs (* Conjonctive semantics *)

let rename f (evs,gs) = List.map f evs, List.map (Expr.rename f) gs

let subst env (evs,gs) =
  let subst_guard e = Eval.subst env e in
  evs, List.map subst_guard gs
