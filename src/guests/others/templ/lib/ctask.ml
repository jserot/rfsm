(* Template for defining the guest language CTask backend *)
(* The implementation defined in this file should match the signature [Guest.CTASK]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax

let pp_typed_symbol fmt (name,t) = (* TO BE WRITTEN *)

let pp_type_expr fmt te =  (* TO BE WRITTEN *)
                    
let pp_type_decl fmt td = (* TO BE WRITTEN *) 

let pp_simple_type fmt t =
  let open Types in 
  match t with
  | TyConstr (c,[]) -> fprintf fmt "%s" c
  | _ -> raise (Unsupported_type t)

let pp_expr fmt e =  (* TO BE WRITTEN *)


