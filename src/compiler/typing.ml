(* Types from type expressions *)

open Types
   
exception Unbound_expr_index of string * Syntax.type_index_expr 

let type_index_of_index_expr e =
  let rec type_index_of = function 
    Syntax.TEConst c -> TiConst c
  | Syntax.TEVar v -> TiVar v
  | Syntax.TEBinop (op,e1,e2) -> TiBinop (op, type_index_of e1, type_index_of e2) in
  type_index_of e.Syntax.ti_desc
    
let rec type_of_type_expr params ~strict:strict te = match te with
  | Syntax.TEBool -> TyBool
  | Syntax.TEEnum cs -> TyEnum cs
  | Syntax.TEInt None -> TyInt None
  | Syntax.TEInt (Some (lo,hi)) -> TyInt (Some (type_index_of_index_expr lo, type_index_of_index_expr hi))
  | Syntax.TEEvent -> TyEvent

and type_of_type_expression params ~strict:strict te = type_of_type_expr params ~strict:strict te.Syntax.te_desc

