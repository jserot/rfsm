module Syntax = Syntax
module Types = Types

module Env = Rfsm.Env
module Annot = Rfsm.Annot
module Location = Rfsm.Location

type env =
  { te_vars: Types.typ_scheme Env.t;
    te_tycons: (int * Types.typ) Env.t;  (** Type constructors, with arity and associated type *)
    te_ctors: Types.typ Env.t;  (** Data constructors, with target type. Ex: "true"->TyBool *)
    te_rfields: (Types.typ * Types.typ) Env.t;  (** Record fields, with source and target types *)  }

let mk_env () =
  { te_vars = Env.init Builtins.typing_env.prims;
    te_tycons = Env.init Builtins.typing_env.tycons;
    te_ctors = Env.init Builtins.typing_env.ctors;
    te_rfields = Env.empty; }

exception Illegal_cast of Syntax.expr
exception Illegal_expr of Location.t * string

let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let lookup_var ~loc v env =
  Types.type_instance @@ lookup ~exc:(Rfsm.Ident.Undefined ("symbol",loc,v)) v env.te_vars
let lookup_tycon ~loc v env =
  lookup ~exc:(Rfsm.Ident.Undefined ("type constructor",loc,v)) v env.te_tycons
let lookup_ctor ~loc v env =
  lookup ~exc:(Rfsm.Ident.Undefined ("value constructor",loc,v)) v env.te_ctors
let lookup_rfield ~loc v env =
  lookup ~exc:(Rfsm.Ident.Undefined ("record field",loc,v)) v env.te_rfields

let add_var ~scope env (v,ty) =
  let ts = match scope with
    | Rfsm.Ident.Global -> Types.generalize ty
    | Rfsm.Ident.Local -> Types.trivial_scheme ty in
  { env with te_vars = Env.add v ts env.te_vars }
let add_param env _ = env (* No parameter in the [simple] guest language *)

let pp_env fmt e = 
  let open Format in
  let pp_tycon fmt (arity,ty) = fprintf fmt "<%d,%a>" arity (Types.pp_typ ~abbrev:false) ty in
  fprintf fmt "@[<v>{@,vars=%a@,tycons=%a@,ctors=%a@,rfields=%a@,}@]@."
    (Env.pp ~sep:":" Types.pp_typ_scheme) e.te_vars
    (Env.pp ~sep:":" pp_tycon) e.te_tycons
    (Env.pp ~sep:":" (Types.pp_typ ~abbrev:false)) e.te_ctors
    (Env.pp ~sep:":" (fun fmt (_,ty) -> fprintf fmt "%a" (Types.pp_typ ~abbrev:true) ty)) e.te_rfields

let add_env exc env (k,v)  =
  if not (Env.mem k env) then Env.add k v env  
  else raise exc

let rec type_of_type_expr env te =
  let ty =
    match te.Annot.desc with
    | Syntax.TeConstr (Rfsm.Ident.{id="array";_},[arg],Some sz) ->
       Types.TyArray (type_of_type_expr env arg, sz)
    | Syntax.TeConstr (c,[],_) ->
       lookup_tycon ~loc:te.Annot.loc c env |> snd
    | Syntax.TeConstr (c,args,_) ->
       Types.TyConstr (c.Rfsm.Ident.id, List.map (type_of_type_expr env) args) in
  te.Annot.typ <- ty;
  ty

let rec type_expression env e =
  let loc = e.Annot.loc in
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup_var ~loc:e.Annot.loc v env
    | Syntax.EInt _ -> Types.type_int ()
    | Syntax.EBool _ -> Types.type_bool ()
    | Syntax.EFloat _ -> Types.type_float ()
    | Syntax.EChar _ -> Types.type_char ()
    | Syntax.EBinop (op,e1,e2) ->
       let ty_fn = lookup_var ~loc:e.Annot.loc op env in
       let ty_args = List.map (type_expression env) [e1;e2] in
       type_application ~loc:e.Annot.loc env ty_fn ty_args
    | Syntax.ECon0 c -> lookup_ctor ~loc:e.Annot.loc c env
    | Syntax.EIndexed (a,i) -> type_indexed_expr ~loc:e.Annot.loc env a i (* shared with type_lhs *)
    | Syntax.ERanged (a,i1,i2) -> type_ranged_expr ~loc:e.Annot.loc env a i1 i2 (* shared with type_lhs *)
    | Syntax.EArrExt [] -> Rfsm.Misc.fatal_error "Typing.type_expression: empty array" (* should not happen *)
    | Syntax.EArrExt ((e1::es) as exps) -> 
       let ty_e1 = type_expression env e1 in
       List.iter (function e -> Types.unify ~loc ty_e1 (type_expression env e)) es;
       Types.type_array ty_e1 (List.length exps)
    | Syntax.ECond (e1,e2,e3) ->
       let ty_e1 = type_expression env e1 in
       let ty_e2 = type_expression env e2 in
       let ty_e3 = type_expression env e3 in
       Types.unify ~loc ty_e1 (Types.type_bool ());
       Types.unify ~loc ty_e2 ty_e3;
       ty_e2
  | Syntax.ECast (e,te) ->
      let ty_e = type_expression env e in
      let ty_t = type_of_type_expr env te in
      type_cast e ty_e ty_t 
  | Syntax.EFapp (f,es) ->
      let ty_args = List.map (type_expression env) es in
      let ty_fn = lookup_var ~loc:e.Annot.loc f env in
      type_application ~loc:e.Annot.loc env ty_fn ty_args
  | Syntax.ERecord (r,f) ->
     begin match lookup_var ~loc:e.Annot.loc r env with
       | TyRecord (_,fs) ->
          begin
            try List.assoc f fs
            with Not_found -> raise (Rfsm.Ident.Undefined ("record field",e.Annot.loc, Rfsm.Ident.mk f))
          end
       | ty ->
          raise (Illegal_expr (e.Annot.loc, "not a record"))
     end 
  | Syntax.ERecordExt [] -> 
     Rfsm.Misc.fatal_error "Typing.type_expression: empty record extension" (* should not happen *)
  | Syntax.ERecordExt ((f,_)::_ as fs) -> 
     let f' = Rfsm.Ident.(mk ~scope:Global f) in
     let _, ty_r = lookup ~exc:(Rfsm.Ident.Undefined ("record field name",e.Annot.loc,Rfsm.Ident.mk f)) f' env.te_rfields in
     let name = match ty_r with
       | TyRecord(name, _) -> name
       | _ -> Rfsm.Misc.fatal_error "Typing.type_expression" in
     let ty_fs = List.map (fun (n,e) -> n, type_expression env e) fs in
     Types.TyRecord (name, ty_fs)
  in
  e.Annot.typ <- ty;
  ty

and type_application ~loc env ty_fn ty_args =
  let open Types in
  let ty_arg = match ty_args with [t] -> t | ts -> TyProduct ts in
  let ty_result = new_type_var () in
  unify ~loc ty_fn (TyArrow (ty_arg,ty_result));
  Types.real_type ty_result

and type_indexed_expr ~loc env a i = 
  let ty_i = type_expression env i in
  Types.unify ~loc:i.Annot.loc ty_i (Types.type_int ());
  match lookup_var ~loc a env with
  | TyConstr ("int", _) -> Types.type_int ()
  | TyArray (t', _) -> t'
  | _ -> raise (Illegal_expr (loc, "only int's and array's can be indexed"))

and type_ranged_expr ~loc env a i1 i2 = 
  let ty_i1 = type_expression env i1 in
  let ty_i2 = type_expression env i2 in
  Types.unify ~loc:i1.Annot.loc ty_i1 (Types.type_int ());
  Types.unify ~loc:i2.Annot.loc ty_i2 (Types.type_int ());
  match lookup_var ~loc a env with
  | TyConstr("int",_) -> Types.type_int () 
  | ty -> raise (Types.Type_conflict (loc, ty, Types.type_int ()))

and type_cast e t1 t2 = match t1, t2 with
  | TyConstr("int",_), TyConstr("int",_)
  | TyConstr("int",_), TyConstr("bool",_)
  | TyConstr("int",_), TyConstr ("char",_)
  | TyConstr ("char",_), TyConstr("int",_)
  | TyConstr("int",_), TyConstr ("float",_)
  | TyConstr("bool",_), TyConstr("bool",_)
  | TyConstr("bool",_), TyConstr("int",_)
  | TyConstr ("float",_), TyConstr ("float",_)
  | TyConstr ("float",_), TyConstr("int",_) -> t2
  | _, _ -> raise (Illegal_cast e)

let type_type_decl env td =
  let add_tycon ty env (name,arity) = add_env (Rfsm.Ident.Duplicate ("type constructor", td.Annot.loc, name)) env (name,(arity,ty)) in
  let add_ctor ty env name =
    let nm = Rfsm.Ident.(mk ~scope:Global name) in
    add_env (Rfsm.Ident.Duplicate ("value constructor", td.Annot.loc, nm)) env (nm,ty) in
  let add_rfield ty lenv (name,te) =
    let ty' = type_of_type_expr env te in 
    let nm = Rfsm.Ident.(mk ~scope:Global name) in
    add_env (Rfsm.Ident.Duplicate ("record field name", td.Annot.loc, nm)) lenv (nm,(ty',ty)) in
  let ty,env' = match td.Annot.desc with
    | Syntax.TD_Enum (name, ctors) -> 
       let ty = Types.TyConstr (name.Rfsm.Ident.id, []) in
       ty,
       { env with te_tycons = add_tycon ty env.te_tycons (name,0);
                  te_ctors = List.fold_left (add_ctor ty) env.te_ctors ctors }
    | Syntax.TD_Record (name, fields) -> 
       let ty = Types.TyRecord (name.Rfsm.Ident.id, List.map (function (n,te) -> (n, type_of_type_expr env te)) fields) in
       ty,
       { env with te_tycons = add_tycon ty env.te_tycons (name,0);
                  te_rfields = List.fold_left (add_rfield ty) env.te_rfields fields }
    | Syntax.TD_Alias (name,te) ->
       let ty = type_of_type_expr env te in 
       ty, 
       { env with te_tycons = add_tycon ty env.te_tycons (name,0) }
  in
  td.Annot.typ <- ty;
  env'

let type_lhs env l =
  let ty = match l.Annot.desc with
    | Syntax.LhsVar x -> lookup_var ~loc:l.Annot.loc x env
    | Syntax.LhsIndex (a,i) -> type_indexed_expr ~loc:l.Annot.loc env a i
    | Syntax.LhsRange (a,i1,i2) -> type_ranged_expr ~loc:l.Annot.loc env a i1 i2
    | Syntax.LhsRField (x,f) -> 
       begin match lookup_var ~loc:l.Annot.loc x env with
       | TyRecord (_,fs) -> 
          begin
            try List.assoc f fs 
            with Not_found -> raise (Rfsm.Ident.Undefined ("record field",l.Annot.loc,Syntax.mk_global_ident f))
          end
       | _ -> Rfsm.Misc.fatal_error "Typing.type_lhs"
       end
  in
  l.Annot.typ <- ty;
  ty

let type_check ~loc ty ty' = Types.unify ~loc ty ty'
