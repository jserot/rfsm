type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr
exception Illegal_index_value of expr

let rec eval e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> Value.Val_int (i,Types.size_of_type e.Rfsm.Annot.typ)
  | Syntax.EBool b -> Value.Val_bool b
  | Syntax.EFloat f -> Value.Val_float f
  | Syntax.EArrExt es -> Value.Val_array (Array.of_list (List.map eval es))
  | _ -> raise (Non_static_value e)

let eval_fn args body = Value.Val_fn (args,body)

let eval_index e = match e.Rfsm.Annot.desc with
  | Syntax.EInt i -> i
  | _ -> raise (Illegal_index_value e)
