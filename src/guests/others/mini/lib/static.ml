(* The implementation defined in this file should match the signature [Guest.STATIC]  specified in ../../../host/lib/guest.ml *)

type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr

let eval e = match e.Rfsm.Annot.desc with
  | Syntax.EBool b -> Value.Val_bool b
  | _ -> raise (Non_static_value e)

let eval_fn args body = 
  Value.Val_unknown  (* No function for thi guest language *)
