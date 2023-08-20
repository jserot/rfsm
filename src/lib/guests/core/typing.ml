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

(* let localize_env env = { env with te_vars = Rfsm.Env.localize env.te_vars } *)
let localize_env env = env

exception Undefined of string * Location.t * Rfsm.Ident.t 
exception Duplicate of string * Location.t * Rfsm.Ident.t

let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let lookup_var ~loc v env =
  try Types.type_instance @@ lookup ~exc:(Undefined ("symbol",loc,v)) v env.te_vars
  with _ -> Format.printf "** env=%a@." (Env.pp ~sep:" : " ~vlayout:true ~qual:true Types.pp_typ_scheme) env.te_vars; raise(Undefined ("symbol",loc,v))
let lookup_ctor ~loc v env =
  lookup ~exc:(Undefined ("value constructor",loc,v)) v env.te_ctors

let add_var env (v,ty) = { env with te_vars = Env.add v (Types.generalize ty) env.te_vars }
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
  (* let ty,env' = match td.Annot.desc with
   *   | Syntax.TD_Enum (name, ctors) -> 
   *      let ty = Types.TyConstr (name,[]) in
   *      let add_ctor env name =
   *        let id = Rfsm.Ident.mk name in
   *        add_env (Duplicate ("value constructor", td.Annot.loc, id)) env (id,ty) in
   *      let add_tycon env (name,arity) =
   *        let id = Rfsm.Ident.mk name in
   *        add_env (Duplicate ("type constructor", td.Annot.loc, id)) env (id,arity) in
   *      ty,
   *      { env with te_tycons = add_tycon env.te_tycons (name,0);
   *                 te_ctors = List.fold_left add_ctor env.te_ctors ctors }
   * in
   * td.Annot.typ <- ty;
   * env' *)

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

let type_lhs env l =
  let ty = match l.Annot.desc with
    | Syntax.LhsVar x -> lookup_var ~loc:l.Annot.loc x env in
  l.Annot.typ <- ty;
  ty

let type_check ~loc ty ty' = Types.unify ~loc ty ty'
