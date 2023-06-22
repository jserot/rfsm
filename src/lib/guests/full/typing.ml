module Syntax = Syntax
module Types = Types

module Env = Rfsm.Env
module Annot = Rfsm.Annot
module Location = Rfsm.Location

type env =
  { te_vars: Types.typ Env.t;
    te_tycons: (int * Types.typ) Env.t;  (** Type constructors, with arity and associated type *)
    te_ctors: Types.typ Env.t;  (** Data constructors, with target type. Ex: "true"->TyBool *)
    te_rfields: (Types.typ * Types.typ) Env.t;  (** Record fields, with source and target types *) 
    te_prims: Types.typ_scheme Env.t }
[@@deriving show {with_path=false}]

let mk_env () =
  { te_vars = Env.empty;
    te_tycons = Env.init Builtins.typing_env.tycons;
    te_ctors = Env.init Builtins.typing_env.ctors;
    te_rfields = Env.empty;
    te_prims = Env.init Builtins.typing_env.prims; }

exception Undefined of string * Location.t * string 
exception Duplicate of string * Location.t * string
exception Illegal_cast of Syntax.expr
exception Illegal_record_access of string * Types.typ * Location.t

let lookup ~exc v env = 
  try Env.find v env 
  with Not_found -> raise exc

let lookup_var ~loc v env = lookup ~exc:(Undefined ("symbol",loc,v)) v env.te_vars

let add_var env (v,ty) = { env with te_vars = Env.add v ty env.te_vars }

let pp_env fmt e = 
  let open Format in
  let pp_tycon fmt (arity,ty) = fprintf fmt "<%d,%a>" arity Types.pp_typ ty in
  fprintf fmt "@[<v>[@,vars=%a@,tycons=%a@,ctors=%a@,prims=%a]@]@."
    (Env.pp ~sep:":" Types.pp_typ) e.te_vars
    (Env.pp ~sep:":" pp_tycon) e.te_tycons
    (Env.pp ~sep:":" Types.pp_typ) e.te_ctors
    (Env.pp ~sep:":" Types.pp_typ_scheme) e.te_prims

let add_env exc env (k,v)  =
  if not (Env.mem k env) then Env.add k v env  
  else raise exc

let rec type_of_type_expr env te =
  let ty =
    match te.Annot.desc with
    | Syntax.TeConstr (c,[],[]) ->
       lookup ~exc:(Undefined ("type constructor",te.Annot.loc,c)) c env.te_tycons |> snd
    | Syntax.TeConstr (c,args,sz) ->
       Types.TyConstr (c, List.map (type_of_type_expr env) args, size_of_size_expr c sz) in
  te.Annot.typ <- Some ty;
  ty

and size_of_size_expr c sz = 
  match c, sz with 
  | "int", [] -> Types.new_size_var ()
  | "array", [] -> Types.new_size_var ()
  | _, [] -> Types.SzNone
  | _, [s] -> Types.SzExpr1 (type_index_of_index_expr s)
  | _, [lo;hi] -> Types.SzExpr2 (type_index_of_index_expr lo, type_index_of_index_expr hi)
  | _, _ -> Rfsm.Misc.fatal_error "Full.Typing.size_of_size_expr" (* Should not happen thx to parsing defns *)

and type_index_of_index_expr e =
  match e.Annot.desc with 
    Syntax.TiConst c -> Types.Index.TiConst c
  | Syntax.TiVar v -> Types.Index.TiVar v
  | Syntax.TiBinop (op,e1,e2) -> Types.Index.TiBinop (op, type_index_of_index_expr e1, type_index_of_index_expr e2)

let rec type_expression env e =
  let loc = e.Annot.loc in
  let type_of e = match e.Annot.typ with
    | Some t -> t
    | None -> Rfsm.Misc.fatal_error "Full.Typing.type_of" in
  let ty = match e.Annot.desc with
    | Syntax.EVar v -> lookup ~exc:(Undefined ("symbol",e.Annot.loc,v)) v env.te_vars
    | Syntax.EInt _ -> Types.type_unsized_int ()
    | Syntax.EBool _ -> Types.type_bool ()
    | Syntax.EFloat _ -> Types.type_float ()
    | Syntax.EChar _ -> Types.type_char ()
    | Syntax.EBinop (op,e1,e2) ->
       let ty_fn = Types.type_instance (lookup ~exc:(Undefined("operator",e.Annot.loc,op)) op env.te_prims) in
       let ty_args = List.map (type_expression env) [e1;e2] in
       type_application ~loc:e.Annot.loc env ty_fn ty_args
    | Syntax.ECon0 c -> lookup ~exc:(Undefined ("value constructor",e.Annot.loc,c)) c env.te_ctors
    | Syntax.EArr (a,i) ->
       begin match lookup ~exc:(Undefined ("symbol",e.Annot.loc,a)) a env.te_vars with
       | TyConstr("int",_,_) -> (* Special case *)
          e.Annot.desc <- Syntax.EBit (a,i);
          Types.type_sized_int 1
       | _ ->
          type_array_access ~loc env a i
       end
    | Syntax.EBit (a,i) -> type_of e  (* Typing has been carried out by the above case *)
    | Syntax.EArrExt [] -> Rfsm.Misc.fatal_error "Full.Typing.type_expression: empty array" (* should not happen *)
    | Syntax.EArrExt ((e1::es) as exps) -> 
       let ty_e1 = type_expression env e1 in
       List.iter (function e -> Types.unify ~loc ty_e1 (type_expression env e)) es;
       Types.type_sized_array ty_e1 (List.length exps)
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
      let env' = 
        { env with te_vars =
                     Env.union
                       env.te_vars
                       (Env.map Types.type_instance env.te_prims) } in 
      let ty_fn = lookup ~exc:(Undefined ("symbol",e.Annot.loc,f)) f env'.te_vars in
      type_application ~loc:e.Annot.loc env' ty_fn ty_args
  | Syntax.ERecord (r,f) ->
     begin match lookup ~exc:(Undefined ("symbol",e.Annot.loc,r)) r env.te_vars with
       | TyRecord (_,fs) ->
          begin
            try List.assoc f fs
            with Not_found -> raise (Undefined ("record field",e.Annot.loc,f))
          end
       | ty ->
          raise (Illegal_record_access (r,ty,e.Annot.loc))
     end 
  | Syntax.ERecordExt fs -> 
     let ty_fs = List.map (fun (n,e) -> n, type_expression env e) fs in
     Types.TyRecord ("", ty_fs) (* Anonymous record *)
  in
  e.Annot.typ <- Some ty;
  ty

and type_application ~loc env ty_fn ty_args =
  let open Types in
  let ty_arg = match ty_args with [t] -> t | ts -> TyProduct ts in
  let ty_result = new_type_var () in
  unify ~loc ty_fn (TyArrow (ty_arg,ty_result));
  Types.real_type ty_result

and type_array_access ~loc env a i =
  let ty_idx = type_expression env i in
  Types.unify ~loc:i.Annot.loc ty_idx (Types.type_unsized_int ());
  let ty_arg = lookup ~exc:(Undefined ("symbol",loc,a)) a env.te_vars in
  let ty_res =
    begin match ty_arg with
    | Types.TyConstr ("array", [t'], _) -> t'
    | _ -> raise (Types.Type_conflict (loc, ty_arg, Types.TyConstr ("array", [], SzNone)))
    end in
  ty_res

and type_cast e t1 t2 = match t1, t2 with
  | TyConstr("int",_,_), TyConstr("int",_,_)
  | TyConstr("int",_,_), TyConstr("bool",_,_)
  | TyConstr("int",_,_), TyConstr ("char",_,_)
  | TyConstr ("char",_,_), TyConstr("int",_,_)
  | TyConstr("int",_,_), TyConstr ("float",_,_)
  | TyConstr("bool",_,_), TyConstr("bool",_,_)
  | TyConstr("bool",_,_), TyConstr("int",_,_)
  | TyConstr ("float",_,_), TyConstr ("float",_,_)
  | TyConstr ("float",_,_), TyConstr("int",_,_) -> t2
  | _, _ -> raise (Illegal_cast e)

let type_type_decl env td =
  let add_tycon ty env (name,arity) = add_env (Duplicate ("type constructor", td.Annot.loc, name)) env (name,(arity,ty)) in
  let add_ctor ty env name = add_env (Duplicate ("value constructor", td.Annot.loc, name)) env (name,ty) in
  let add_rfield ty lenv (name,te) =
    let ty' = type_of_type_expr env te in 
    add_env (Duplicate ("record field name", td.Annot.loc, name)) lenv (name,(ty',ty)) in
  let ty,env' = match td.Annot.desc with
    | Syntax.TD_Enum (name, ctors) -> 
       let ty = Types.TyConstr (name, [], SzNone) in
       ty,
       { env with te_tycons = add_tycon ty env.te_tycons (name,0);
                  te_ctors = List.fold_left (add_ctor ty) env.te_ctors ctors }
    | Syntax.TD_Record (name, fields) -> 
       let ty = Types.TyRecord (name, List.map (function (n,te) -> (n, type_of_type_expr env te)) fields) in
       ty,
       { env with te_tycons = add_tycon ty env.te_tycons (name,0);
                  te_rfields = List.fold_left (add_rfield ty) env.te_rfields fields }
    | Syntax.TD_Alias (name,te) ->
       let ty = type_of_type_expr env te in 
       ty, 
       { env with te_tycons = add_tycon ty env.te_tycons (name,0) }
  in
  td.Annot.typ <- Some ty;
  env'

let type_lhs env l =
  let ty = match l.Annot.desc with
    | Syntax.LhsVar x -> lookup ~exc:(Undefined ("symbol",l.Annot.loc,x)) x env.te_vars
    | Syntax.LhsArrInd (a,i) -> type_array_access ~loc:l.Annot.loc env a i
    | Syntax.LhsRField (x,f) -> 
       begin match lookup ~exc:(Undefined ("symbol",l.Annot.loc,x)) x env.te_vars with
       | TyRecord (_,fs) -> 
          begin
            try List.assoc f fs 
            with Not_found -> raise (Undefined ("record field",l.Annot.loc,f))
          end
       | _ -> Rfsm.Misc.fatal_error "Full.Typing.type_lhs"
       end
  in
  l.Annot.typ <- Some ty;
  ty

let type_check ~loc ty ty' = Types.unify ~loc ty ty'
