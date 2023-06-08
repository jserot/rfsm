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

and lhs = { mutable l_desc: lhs_desc }

and lhs_desc = 
  | LhsVar of string                         (* v := ... *)
  | LhsArrInd of string * Expr.t             (* v[i] := ... when v is an array *)
  | LhsArrRange of string * Expr.t * Expr.t  (* v[hi:lo] := ... when v is an int *)
  | LhsRField of string * string             (* v.field_name when v has a record type *)
  (* TO BE EXTENDED if needed ... *)
          
let mk_lhs v = {l_desc = LhsVar v}

let lhs_name l = match l.l_desc with
  | LhsVar v -> v
  | LhsArrInd (v,_) -> v
  | LhsArrRange (v,_,_) -> v
  | LhsRField (v,_) -> v
                
let vars_of a = match a with
  | Assign ({l_desc=LhsVar v},e) -> Expr.vars_of e, Expr.VarSet.singleton v 
  | Assign ({l_desc=LhsArrInd (a,i)}, e)  -> Expr.VarSet.union (Expr.vars_of i) (Expr.vars_of e), Expr.VarSet.singleton a
  | Assign ({l_desc=LhsArrRange (a,i1,i2)}, e) ->
     Expr.VarSet.union (Expr.VarSet.union (Expr.vars_of i1) (Expr.vars_of e)) (Expr.vars_of i2),
     Expr.VarSet.singleton a
  | Assign ({l_desc=LhsRField (v,_)},e) -> Expr.vars_of e, Expr.VarSet.singleton v 
  | Emit e -> Expr.VarSet.empty, Expr.VarSet.singleton e
  | StateMove _ -> Expr.VarSet.empty, Expr.VarSet.empty
                 
let string_of_lhs l = match l.l_desc with
  | LhsVar v -> v
  | LhsArrInd (a,i) -> a ^ "[" ^ Expr.to_string i ^ "]"
  | LhsArrRange (a,i1,i2) -> a ^ "[" ^ Expr.to_string i1 ^ ":" ^ Expr.to_string i2 ^ "]"
  | LhsRField (a,f) -> a ^ "." ^ f
                
let to_string a = match a with
  | Assign (r, expr) -> string_of_lhs r ^ ":=" ^ Expr.to_string expr
  | Emit id -> id
  | StateMove (id, s,s') -> s ^ "->" ^ s'

let rename f a = match a with
  | Assign ({l_desc=LhsVar v}, e) -> Assign ({l_desc=LhsVar (f v)}, Expr.rename f e)
  | Assign ({l_desc=LhsArrInd (a,i)}, e) -> Assign ({l_desc=LhsArrInd(f a, Expr.rename f i)}, Expr.rename f e)
  | Assign ({l_desc=LhsArrRange (a,i1,i2)}, e) -> Assign ({l_desc=LhsArrRange(f a, Expr.rename f i1, Expr.rename f i2)}, Expr.rename f e)
  | Assign ({l_desc=LhsRField (v,f')}, e) -> Assign ({l_desc=LhsRField (f v, f')}, Expr.rename f e)
  | Emit e -> Emit (f e)
  | StateMove _ -> a

let subst env act = match act with
  | Assign ({l_desc=LhsVar v}, e) -> Assign ({l_desc=LhsVar v}, Eval.subst env e)
  | Assign ({l_desc=LhsArrInd (a,i)}, e) -> Assign ({l_desc=LhsArrInd (a, Eval.subst env i)}, Eval.subst env e)
  | Assign ({l_desc=LhsArrRange (a,i1,i2)}, e) -> Assign ({l_desc=LhsArrRange (a, Eval.subst env i1, Eval.subst env i2)}, Eval.subst env e)
  | Assign ({l_desc=LhsRField (v,f)}, e) -> Assign ({l_desc=LhsRField (v,f)}, Eval.subst env e)
  | act -> act
                
