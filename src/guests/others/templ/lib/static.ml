(* Template for defining the guest language static representation *)
(* The implementation defined in this file should match the signature [Guest.STATIC]  specified in ../../../host/lib/guest.ml *)

type expr = Syntax.expr
type value = Value.t

exception Non_static_value of expr

let eval e = (* TO BE WRITTEN *)

let eval_fn args body =  (* TO BE WRITTEN *)
