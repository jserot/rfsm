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

type t =
    Assign of lhs * Expr.t
  | Emit of string
  | StateMove of string * string * string

and lhs =
  | Var0 of string             (* v := ... *)
  | Var1 of string * Expr.t    (* v[i] := ... *)
  (* TO BE EXTENDED if needed ... *)
          
let lhs_name = function
  | Var0 v -> v
  | Var1 (v,_) -> v
                
let vars_of a = match a with
  | Assign (Var0 v,e) -> Expr.vars_of e, Expr.VarSet.singleton v 
  | Assign (Var1 (a,i), e) -> Expr.VarSet.union (Expr.vars_of i) (Expr.vars_of e), Expr.VarSet.singleton a
  | Emit e -> Expr.VarSet.empty, Expr.VarSet.singleton e
  | StateMove _ -> Expr.VarSet.empty, Expr.VarSet.empty
                 
let string_of_lhs = function
  | Var0 v -> v
  | Var1 (a,i) -> a ^ "[" ^ Expr.to_string i ^ "]"
                
let to_string a = match a with
  | Assign (r, expr) -> string_of_lhs r ^ ":=" ^ Expr.to_string expr
  | Emit id -> id
  | StateMove (id, s,s') -> s ^ "->" ^ s'

let rename f a = match a with
  | Assign (Var0 v, e) -> Assign (Var0 (f v), Expr.rename f e)
  | Assign (Var1 (a,i), e) -> Assign (Var1(f a, Expr.rename f i), Expr.rename f e)
  | Emit e -> Emit (f e)
  | StateMove _ -> a

let subst env act = match act with
  | Assign (Var0 v, e) -> Assign (Var0 v, Eval.subst env e)
  | Assign (Var1 (a,i), e) -> Assign (Var1 (a, Eval.subst env i), Eval.subst env e)
  | act -> act
                
