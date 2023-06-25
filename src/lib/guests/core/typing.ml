module Syntax = Syntax
module Types = Types

module Env = Rfsm.Env
module Annot = Rfsm.Annot
module Location = Rfsm.Location

type env =
  { te_vars: Types.typ Env.t;
    te_ctors: Types.typ Env.t;  (** Data constructors, with target type. Ex: "true"->TyBool *)
    te_tycons: int Env.t;  (** Type constructors, with arity. Ex: "array"->(1, 'a array) *)
    te_prims: Types.typ_scheme Env.t }

let mk_env () =
  { te_vars = Env.empty;
    te_ctors = Env.init Builtins.typing_env.ctors;
    te_tycons = Env.init Builtins.typing_env.tycons;
    te_prims = Env.init Builtins.typing_env.prims; }

exception Undefined of string * Location.t * string 
exception Duplicate of string * Location.t * string

let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let lookup_var ~loc v env = lookup ~exc:(Undefined ("symbol",loc,v)) v env.te_vars

let add_var env (v,ty) = { env with te_vars = Env.add v ty env.te_vars }

let pp_env fmt e = 
  let open Format in
  fprintf fmt "@[<v>[@,vars=%a@,ctors=%a@,tycons=%a@,prims=%a]@]@."
    (Env.pp ~sep:" : " (Types.pp_typ ~abbrev:false)) e.te_vars
    (Env.pp ~sep:" : " (Types.pp_typ ~abbrev:false)) e.te_ctors
    (Env.pp ~sep:" : " pp_print_int) e.te_tycons
    (Env.pp ~sep:" : " Types.pp_typ_scheme) e.te_prims

let add_env exc env (k,v)  =
  if not (Env.mem k env) then Env.add k v env  
  else raise exc

let type_type_decl env td =
  let ty,env' = match td.Annot.desc with
    | Syntax.TD_Enum (name, ctors) -> 
       let ty = Types.TyConstr (name,[]) in
       let add_ctor env name = add_env (Duplicate ("value constructor", td.Annot.loc, name)) env (name,ty) in
       let add_tycon env (name,arity) = add_env (Duplicate ("type constructor", td.Annot.loc, name)) env (name,arity) in
       ty,
       { env with te_tycons = add_tycon env.te_tycons (name,0);
                  te_ctors = List.fold_left add_ctor env.te_ctors ctors }
  in
  td.Annot.typ <- Some ty;
  env'

let rec type_of_type_expr env te =
  let ty = match te.Annot.desc with
    | Syntax.TeConstr c -> Types.TyConstr (c, List.map (type_of_type_expr env) []) in
  te.Annot.typ <- Some ty;
  ty

let rec type_expression env e =
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup ~exc:(Undefined ("symbol",e.Annot.loc,v)) v env.te_vars
    | Syntax.EInt _ -> Types.TyConstr ("int", [])
    | Syntax.EBool _ -> Types.TyConstr ("bool", [])
    | Syntax.EBinop (op,e1,e2) ->
      let ty_fn = Types.type_instance (lookup ~exc:(Undefined("operator",e.Annot.loc,op)) op env.te_prims) in
      let ty_args = List.map (type_expression env) [e1;e2] in
      type_application e env ty_fn ty_args
    | Syntax.ECon0 c -> lookup ~exc:(Undefined ("value constructor",e.Annot.loc,c)) c env.te_ctors in
  e.Annot.typ <- Some ty;
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
  let ty_arg = lookup ~exc:(Undefined ("symbol",loc,a)) a env.te_vars in
  let ty_res =
    begin match ty_arg with
    | Types.TyConstr ("array", [t']) -> t'
    | _ -> raise (Types.Type_conflict (loc, ty_arg, Types.TyConstr ("array", [])))
    end in
  ty_res

let type_lhs env l =
  let ty = match l.Annot.desc with
    | Syntax.LhsVar x -> lookup ~exc:(Undefined ("symbol",l.Annot.loc,x)) x env.te_vars in
  l.Annot.typ <- Some ty;
  ty

let type_check ~loc ty ty' = Types.unify ~loc ty ty'
