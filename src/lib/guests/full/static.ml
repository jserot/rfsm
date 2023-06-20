type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr

let rec eval e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> Value.Val_int i
  | Syntax.EBool b -> Value.Val_bool b
  | Syntax.EFloat f -> Value.Val_float f
  | Syntax.EArrExt es -> Value.Val_array (Array.of_list (List.map eval es))
  | _ -> raise (Non_static_value e)
