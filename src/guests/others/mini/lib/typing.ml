(* The implementation defined in this file should match the signature [Guest.TYPING]  specified in ../../../host/lib/guest.ml *)

module Syntax = Syntax
module Types = Types

module Env = Rfsm.Env
module Annot = Rfsm.Annot
module Location = Rfsm.Location

type env = Types.typ Env.t

let mk_env () = Env.init []

let lookup_var ~loc v env = 
  try Env.find v env 
  with Not_found -> raise (Rfsm.Ident.Undefined("symbol",loc,v))

let add_var ~scope env (v,ty) = Env.add v ty env

let add_param env (p,e) =  env (* There's no model parameter in this guest language *)

let pp_env fmt env = Env.pp ~sep:" : " (Types.pp_typ ~abbrev:false) fmt env

let type_type_decl env td = env

let type_of_type_expr env te =
  let ty = match te.Rfsm.Annot.desc with
    | Syntax.TeConstr "event" -> Types.TyEvent
    | Syntax.TeConstr "bool" -> Types.TyBool
    | Syntax.TeConstr _ -> failwith "Undefined type constructor" in
  te.Annot.typ <- ty;
  ty

let type_expression env e =
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup_var ~loc:e.Annot.loc v env 
    | Syntax.EBool _ -> Types.TyBool  in
  e.Annot.typ <- ty;
  ty

let type_lval env l =
  let ty = match l.Annot.desc with
    | x -> lookup_var ~loc:l.Annot.loc x env in
  l.Annot.typ <- ty;
  ty

exception Type_conflict of Location.t * Types.typ * Types.typ

let type_check ~loc ty ty' =
  let open Types in
  match ty, ty' with
  | TyBool, TyBool -> ()
  | TyEvent, TyEvent -> ()
  | _, _ -> raise (Type_conflict(loc,ty,ty'))
  
