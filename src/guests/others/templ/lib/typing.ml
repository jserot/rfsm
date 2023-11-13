(* Template for defining the guest language typing engine *)
(* The implementation defined in this file should match the signature [Guest.TYPING]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Types = Types

type env =  (* TO BE FILLED *)

let mk_env () = (* TO BE FILLED *)

let lookup_var ~loc v env = (* TO BE FILLED *)

let add_var ~scope env (v,ty) = (* TO BE FILLED *)

let add_param env (p,e) =  (* TO BE FILLED *)

let pp_env fmt e =  (* TO BE FILLED *)

let type_type_decl env td =  (* TO BE FILLED *)

let rec type_of_type_expr env te = (* TO BE FILLED *)

let rec type_expression env e = (* TO BE FILLED *)

let type_lhs env l = (* TO BE FILLED *)

let type_check ~loc ty ty' =  (* TO BE FILLED. Hint: should probably call sth like [Types.unify ~loc ty ty'] *)
