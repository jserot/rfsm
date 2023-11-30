(* Template for defining the guest language VHDL backend *)
(* The implementation defined in this file should match the signature [Guest.VHDL]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Static = Static

type value = Value.t

exception Unsupported_type of Syntax.Types.typ
exception Unsupported_expr of Syntax.expr
exception Unsupported_value of Value.t

let vhdl_type_of t =
  let open Rfsm.Vhdl_types in
  match t with 
    (* TO BE FILLED *)
    | _ -> raise (Unsupported_type t)

let allowed_shared_type ty =  (* TO BE FILLED. Hint: result is probably [true] for simple scalar types (int, bool, ...) *)

let pp_ident = Rfsm.Ident.pp 

let pp_typ fmt ~type_mark t = Rfsm.Vhdl_types.pp ~type_mark fmt (vhdl_type_of t)
let pp_abbr_typ fmt t = pp_typ fmt ~type_mark:Rfsm.Vhdl_types.TM_Abbr t
let pp_full_typ fmt t = pp_typ fmt ~type_mark:Rfsm.Vhdl_types.TM_Full t

let pp_type_expr fmt ~type_mark te = pp_typ fmt ~type_mark te.Rfsm.Annot.typ
let pp_abbr_type_expr fmt t = pp_type_expr fmt ~type_mark:Rfsm.Vhdl_types.TM_Abbr t
let pp_full_type_expr fmt t = pp_type_expr fmt ~type_mark:Rfsm.Vhdl_types.TM_Full t

let rec pp_expr fmt e =  (* TO BE FILLED *)

and pp_bool fmt b = match Rfsm.Vhdl_types.cfg.vhdl_bool_as_bool, b with
  | true, _ -> fprintf fmt "%b" b
  | false, true -> fprintf fmt "'1'"
  | false, false -> fprintf fmt "'0'"
                  
let rec pp_lval_desc fmt l = (* TO BE FILLED *)
and pp_lval fmt l = pp_lval_desc fmt l.Rfsm.Annot.desc

let pp_type_decl fmt td =  (* TO BE FILLED *)
let pp_type_fns_intf fmt td =  (* TO BE FILLED *)
let pp_type_fns_impl fmt td =  (* TO BE FILLED *)

let pp_value fmt (v,_) =  (* TO BE FILLED *)

