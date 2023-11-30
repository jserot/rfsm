(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

module Syntax = Syntax
module Types = Types

module Env = Rfsm.Env
module Annot = Rfsm.Annot
module Location = Rfsm.Location

type env =
  { te_vars: Types.typ_scheme Env.t;
    te_ctors: Types.typ Env.t;  (** Data constructors, with target type. Ex: "true"->TyBool *)
    te_tycons: int Env.t;  (** Type constructors, with arity. Ex: "int", "bool", "event" *) }

let mk_env () =
  { te_vars = Env.init Builtins.typing_env.prims;
    te_ctors = Env.init Builtins.typing_env.ctors;
    te_tycons = Env.init Builtins.typing_env.tycons; }

let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let lookup_var ~loc v env =
  try Types.type_instance @@ lookup ~exc:(Rfsm.Ident.Undefined ("symbol",loc,v)) v env.te_vars
  with _ -> Format.printf "** env=%a@." (Env.pp ~sep:" : " ~vlayout:true ~qual:true Types.pp_typ_scheme) env.te_vars; raise(Rfsm.Ident.Undefined ("symbol",loc,v))
let lookup_ctor ~loc v env =
  lookup ~exc:(Rfsm.Ident.Undefined ("value constructor",loc,v)) v env.te_ctors

let add_var ~scope env (v,ty) =
  let ts = match scope with
    | Rfsm.Ident.Global -> Types.generalize ty
    | Rfsm.Ident.Local -> Types.trivial_scheme ty in
  { env with te_vars = Env.add v ts env.te_vars }
let add_param env _ = env (* No parameter in the [core] guest language *)

let pp_env fmt e = 
  let open Format in
  fprintf fmt "@[<v>[@,vars=%a@,ctors=%a@,tycons=%a@]@]@."
    (Env.pp ~sep:" : " Types.pp_typ_scheme) e.te_vars
    (Env.pp ~sep:" : " (Types.pp_typ ~abbrev:false)) e.te_ctors
    (Env.pp ~sep:" : " pp_print_int) e.te_tycons

let add_env exc env (k,v)  =
  if not (Env.mem k env) then Env.add k v env  
  else raise exc

let type_type_decl env td = env (* No type declaration in the core language *)

let rec type_of_type_expr env te =
  let ty = match te.Annot.desc with
    | Syntax.TeConstr c -> Types.TyConstr (c, List.map (type_of_type_expr env) []) in
  te.Annot.typ <- ty;
  ty

let rec type_expression env e =
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup_var ~loc:e.Annot.loc v env
    | Syntax.EInt _ -> Types.TyConstr ("int", [])
    | Syntax.EBool _ -> Types.TyConstr ("bool", [])
    | Syntax.EBinop (op,e1,e2) ->
      let ty_fn = lookup_var ~loc:e.Annot.loc op env in
      let ty_args = List.map (type_expression env) [e1;e2] in
      type_application e env ty_fn ty_args
    | Syntax.ECon0 c -> lookup_ctor ~loc:e.Annot.loc c env in
  e.Annot.typ <- ty;
  ty

and type_application expr env ty_fn ty_args =
  let open Types in
  let ty_arg = TyProduct ty_args in
  let ty_result = new_type_var () in
  unify ~loc:expr.Annot.loc ty_fn (TyArrow (ty_arg,ty_result));
  Types.real_type ty_result

and type_array_access ~loc env a i =
  let ty_idx = type_expression env i in
  Types.unify ~loc:i.Annot.loc ty_idx (Types.TyConstr ("int", []));
  let ty_arg = lookup_var ~loc a env in
  let ty_res =
    begin match ty_arg with
    | Types.TyConstr ("array", [t']) -> t'
    | _ -> raise (Types.Type_conflict (loc, ty_arg, Types.TyConstr ("array", [])))
    end in
  ty_res

let type_lval env l =
  let ty = match l.Annot.desc with
    | Syntax.LvalVar x -> lookup_var ~loc:l.Annot.loc x env in
  l.Annot.typ <- ty;
  ty

let type_check ~loc ty ty' = Types.unify ~loc ty ty'
