(* The implementation defined in this file should match the signature [Guest.SYSTEMC]  specified in ../../../host/lib/guest.ml *)

open Format 

module Syntax = Syntax
module Static = Static
module Annot = Rfsm.Annot

type value = Value.t

let pp_type_expr fmt te = 
  match te.Annot.desc with
  | Syntax.TeConstr "event" -> fprintf fmt "bool"  (* Events are implemented as boolean signals here *)
  | Syntax.TeConstr c -> fprintf fmt "%s" c 

let pp_typ fmt t = 
  match t with
  | Types.TyEvent -> fprintf fmt "bool" (* Events are implemented as boolean signals here *)
  | Types.TyBool -> fprintf fmt "bool" 
  | Types.TyUnknown -> fprintf fmt "???"

let pp_expr fmt e = Syntax.pp_expr fmt e

let pp_lhs fmt l = Syntax.pp_lhs fmt l 

let pp_value fmt v = Value.pp fmt v

let pp_typed_symbol fmt (name,t) = 
  fprintf fmt "%a %a" pp_typ t.Syntax.Annot.typ Rfsm.Ident.pp name 

let pp_type_decl fmt td = () (* No type declarations for this guest language *)

and pp_type_impl fmt td = () (* No type declarations for this guest language *)

