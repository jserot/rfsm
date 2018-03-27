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

(* Types from type expressions *)

open Types
   
exception Unbound_expr_index of string * Syntax.type_index_expr 

let type_index_of_index_expr e =
  let rec type_index_of = function 
    Syntax.TEConst c -> TiConst c
  | Syntax.TEVar v -> TiVar v
  | Syntax.TEBinop (op,e1,e2) -> TiBinop (op, type_index_of e1, type_index_of e2) in
  type_index_of e.Syntax.ti_desc
    
exception Unbound_type_ctor of string
                             
let rec type_of_type_expr defns params ~strict:strict te = match te with
  | Syntax.TEBool -> TyBool
  | Syntax.TEInt None -> TyInt None
  | Syntax.TEInt (Some (lo,hi)) -> TyInt (Some (type_index_of_index_expr lo, type_index_of_index_expr hi))
  | Syntax.TEEvent -> TyEvent
  | Syntax.TEName n ->
     try List.assoc n defns
     with Not_found -> raise (Unbound_type_ctor n)

and type_of_type_expression defns params ~strict:strict te =
  type_of_type_expr defns params ~strict:strict te.Syntax.te_desc

