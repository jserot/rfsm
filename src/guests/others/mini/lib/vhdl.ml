(* Template for defining the guest language VHDL backend *)
(* The implementation defined in this file should match the signature [Guest.VHDL]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Static = Static
module Ident = Rfsm.Ident
module Annot = Rfsm.Annot

type value = Value.t

exception Unsupported_type of Syntax.Types.typ
exception Unsupported_value of Value.t

let vhdl_type_of t =
  let open Rfsm.Vhdl_types in
  match t with 
    | Types.TyEvent -> Std_logic
    | Types.TyBool -> Std_logic
    | _ -> raise (Unsupported_type t)

let allowed_shared_type ty = true

let pp_ident = Rfsm.Ident.pp 

let pp_bool fmt b = Format.fprintf fmt "'%d'" (if b then 1 else 0) (* bool as std_logic *)

let pp_expr fmt e = 
  match e.Annot.desc with
  | Syntax.EBool b -> pp_bool fmt b 
  | Syntax.EVar v -> Format.fprintf fmt "%a=%a" Ident.pp v pp_bool true

let pp_lval fmt l = Syntax.pp_lval fmt l

let pp_type_decl fmt td = () (* No type declarations for this guest language *)
let pp_type_fns_intf fmt td =  ()
let pp_type_fns_impl fmt td =  ()

let pp_value fmt (v,_) =
  match v with
  | Value.Val_bool v -> pp_bool fmt v 
  | _ -> raise (Unsupported_value v)

