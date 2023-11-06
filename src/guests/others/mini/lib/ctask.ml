(* The implementation defined in this file should match the signature [Guest.CTASK]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Ident = Rfsm.Ident
module Annot = Rfsm.Annot

let pp_typed_symbol fmt (name,t) =
  Format.fprintf fmt "%a %a" (Types.pp_typ ~abbrev:false) t.Syntax.Annot.typ Ident.pp name 

let pp_type_expr fmt te =  
  match te.Annot.desc with
  | Syntax.TeConstr c -> Format.fprintf fmt "%s" c 
                    
let pp_type_decl fmt td = () (* No type declarations for this guest language *)

let pp_expr fmt e = Syntax.pp_expr fmt e


