type t =
    Assign of string * Expr.t
  | Emit of string
  | StateMove of string * string * string

let vars_of a = match a with
  | Assign (v,e) -> Expr.vars_of e, Expr.VarSet.singleton v 
  | Emit e -> Expr.VarSet.empty, Expr.VarSet.singleton e
  | StateMove _ -> Expr.VarSet.empty, Expr.VarSet.empty
                 
let to_string a = match a with
  | Assign (id, expr) -> id ^ ":=" ^ Expr.to_string expr
  | Emit id -> id
  | StateMove (id, s,s') -> s ^ "->" ^ s' (* should not happen *)

let rename f a = match a with
  | Assign (v,e) -> Assign (f v, Expr.rename f e)
  | Emit e -> Emit (f e)
  | StateMove _ -> a

let subst env act = match act with
  | Assign (i,e) -> Assign (i, Expr.subst env e)
  | act -> act
                
