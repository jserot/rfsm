module Syntax = Syntax
module Types = Types

module Env = Msic.Env
module Annot = Msic.Annot
module Location = Msic.Location

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

let pp_env fmt e = 
  let open Format in
  fprintf fmt "@[<v>[@,vars=%a@,ctors=%a@,tycons=%a@,prims=%a]@]@."
    (Env.pp ~sep:" : " Types.pp_typ) e.te_vars
    (Env.pp ~sep:" : " Types.pp_typ) e.te_ctors
    (Env.pp ~sep:" : " pp_print_int) e.te_tycons
    (Env.pp ~sep:" : " Types.pp_typ_scheme) e.te_prims

exception Typing_error of Location.t * Types.typ * Types.typ
exception Undefined of string * Location.t * string 
exception Duplicate of string * Location.t * string

let add_env exc env (k,v)  =
  if not (Env.mem k env) then Env.add k v env  
  else raise exc

let type_type_decl env td =
  let ty,env' = match td.Annot.desc with
    | Syntax.TD_Enum (name, ctors) -> 
       let ty = Types.TyConstr (name,[],None) in
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
    | Syntax.TeConstr (c,args,sz) -> Types.TyConstr (c, List.map (type_of_type_expr env) args, sz) in
  te.Annot.typ <- Some ty;
  ty
  
let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let rec type_expr env e =
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup ~exc:(Undefined ("symbol",e.Annot.loc,v)) v env.te_vars
    | Syntax.EInt _ -> Types.TyConstr ("int", [], None)
    | Syntax.EBool _ -> Types.TyConstr ("bool", [], None)
    | Syntax.ECon0 c -> lookup ~exc:(Undefined ("value constructor",e.Annot.loc,c)) c env.te_ctors
    | Syntax.EBinop (op,e1,e2) ->
      let ty_fn = Types.type_instance (lookup ~exc:(Undefined("operator",e.Annot.loc,op)) op env.te_prims) in
      let ty_args = List.map (type_expr env) [e1;e2] in
      type_application e env ty_fn ty_args 
    | Syntax.EArr (a,i) -> type_array_access ~loc:e.Annot.loc env a i in
  e.Annot.typ <- Some ty;
  ty

and type_array_access ~loc env a i =
  let ty_idx = type_expr env i in
  Types.unify ~loc:i.Annot.loc ty_idx (Types.TyConstr ("int", [], None));
  let ty_arg = lookup ~exc:(Undefined ("symbol",loc,a)) a env.te_vars in
  let ty_res =
    begin match ty_arg with
    | Types.TyConstr ("array", [t'], _) -> t'
    | _ -> raise (Typing_error (loc, ty_arg, Types.TyConstr ("array", [], None)))
    end in
  ty_res

and type_application expr env ty_fn ty_args =
  let open Types in
  let ty_arg = TyProduct ty_args in
  let ty_result = new_type_var () in
  unify ~loc:expr.Annot.loc ty_fn (TyArrow (ty_arg,ty_result));
  Types.real_type ty_result

let type_lhs env l =
  let ty = match l.Annot.desc with
    | Syntax.LhsVar x -> lookup ~exc:(Undefined ("symbol",l.Annot.loc,x)) x env.te_vars
    | Syntax.LhsArrInd (a,i) -> type_array_access ~loc:l.Annot.loc env a i  in
  l.Annot.typ <- Some ty;
  ty

let type_var_decl env d =
   match d.Annot.desc with
   | (x,te) -> 
       let ty = type_of_type_expr env te in
       te.Annot.typ <- Some ty;
       { env with te_vars = Env.add x ty env.te_vars }

let type_action env a =
  let ty = match a.Annot.desc with
  | Syntax.Assign (lhs,e) ->
     let t = type_lhs env lhs in
     let t' = type_expr env e in
     Types.unify ~loc:a.Annot.loc t t';
     Types.TyConstr ("unit", [], None) in
  a.Annot.typ <- Some ty;
  env
