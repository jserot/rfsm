(* Template for defining the guest language SystemC backend *)
(* The implementation defined in this file should match the signature [Guest.SYSTEMC]  specified in ../../../host/lib/guest.ml *)

open Format 

module Syntax = Syntax
module Static = Static

type value = Value.t

exception Unsupported_type of Syntax.Types.typ
exception Unsupported_expr of Syntax.expr
exception Unsupported_value of Value.t

let pp_type_expr fmt te =  (* TO BE FILLED *)

let pp_typ fmt t = (* TO BE FILLED *)

let pp_expr fmt e =  (* TO BE FILLED *)

let rec pp_lhs_desc fmt l = (* TO BE FILLED *)
and pp_lhs fmt l = pp_lhs_desc fmt l.Rfsm.Annot.desc

let pp_value fmt v =  (* TO BE FILLED *)

let pp_typed_symbol fmt (name,t) = (* TO BE FILLED *)

let pp_type_decl fmt td = (* TO BE FILLED *)

and pp_type_impl fmt td = (* TO BE FILLED *)

