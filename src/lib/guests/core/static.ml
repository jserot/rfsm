type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr

let eval e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> Value.Val_int i
  | Syntax.EBool b -> Value.Val_bool b
  | _ -> raise (Non_static_value e)