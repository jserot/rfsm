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

let pp_typ fmt ~type_mark t = Rfsm.Vhdl_types.pp ~type_mark fmt (vhdl_type_of t)
let pp_abbr_typ fmt t = pp_typ fmt ~type_mark:Rfsm.Vhdl_types.TM_Abbr t
let pp_full_typ fmt t = pp_typ fmt ~type_mark:Rfsm.Vhdl_types.TM_Full t

let pp_type_expr fmt ~type_mark te = pp_typ fmt ~type_mark te.Rfsm.Annot.typ
let pp_abbr_type_expr fmt t = pp_type_expr fmt ~type_mark:Rfsm.Vhdl_types.TM_Abbr t
let pp_full_type_expr fmt t = pp_type_expr fmt ~type_mark:Rfsm.Vhdl_types.TM_Full t

let pp_bool fmt b = Format.fprintf fmt "'%d'" (if b then 1 else 0) (* bool as std_logic *)

let pp_expr fmt e = 
  match e.Annot.desc with
  | Syntax.EBool b -> pp_bool fmt b 
  | Syntax.EVar v -> Format.fprintf fmt "%a=%a" Ident.pp v pp_bool true

let pp_lhs fmt l = Syntax.pp_lhs fmt l

let pp_type_decl fmt td = () (* No type declarations for this guest language *)
let pp_type_fns_intf fmt td =  ()
let pp_type_fns_impl fmt td =  ()

let pp_value fmt (v,_) =
  match v with
  | Value.Val_bool v -> pp_bool fmt v 
  | _ -> raise (Unsupported_value v)

