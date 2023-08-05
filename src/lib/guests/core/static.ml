type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr
exception Illegal_index_value of expr

let eval e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> Value.Val_int i
  | Syntax.EBool b -> Value.Val_bool b
  | _ -> raise (Non_static_value e)

let eval_fn args body = Value.Val_unknown  (* No function for the Core language *)

let eval_index e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> i
  | _ -> raise (Illegal_index_value e)
