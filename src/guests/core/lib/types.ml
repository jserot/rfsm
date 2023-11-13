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

(* NOTE. Even this simple` guest language needs a polymorphic type system, in order
   to accomodate primitives such as [=], [<], etc., which must have type : [t * t -> bool] for any possible type [t].
   This is classicaly supported by using _type schemes_. The type scheme assigned to [=] for ex is 
   [for all 'a. 'a * 'a -> bool] and any time this primitive is referenced in the program, this type
   scheme is _instanciated_ to a new type ['v * 'v -> bool], where ['v] is a fresh type variable.

   Note that this type of polymorphism only occurs for _builtin primitives_. There's no way to
   declare a constant, function, global IO, model IO or local variable with a polymorphic type (in other
   words, type expressions never contain type variables.

   The generalisation/instanciation operations are simpler than in the classical Hindley-Milner type system
   since it either involves __all_ or _none__ of the type variables occuring in a type. The list of variables to
   be generalized in a type can be computed once for all (since it does not depend of the context of
   the expression in which this type occurs). Generalisation extracts all type and size variables and
   put them in the list(s) associated to the resulting type scheme. Instanciation systematically
   replaces all the variables of these lists by fresh copies. *)

module Location = Rfsm.Location
                
type typ =
   | TyVar of ty_var
   | TyArrow of typ * typ
   | TyProduct of typ list         
   | TyConstr of string * typ list (** name, args *)

and ty_var =
  { stamp: int; (* for debug *)
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_params: ty_var list;
    ts_body: typ }

let no_type = TyProduct []

exception Type_circularity of Location.t * typ * typ
exception Type_conflict of Location.t * typ * typ

let rec type_repr = function
  | TyVar({value = Known ty1; _} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

let rec real_type ty = 
  match type_repr ty with
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyVar { value=Known ty'; _} -> ty'
  | ty -> ty

let type_arrow t1 t2 = TyArrow (t1, t2)
let type_arrow2 t1 t2 t3 = type_arrow t1 (type_arrow t2 t3)
let type_constr c ts = TyConstr(c, ts)
let type_pair t1 t2 = TyProduct [t1;t2]
let type_product = function
  | [t] -> t
  | ts -> TyProduct ts
let type_int () = type_constr "int" [] 
let type_bool () = type_constr "bool" []
let mk_type_constr0 c = type_constr c []
let is_type_constr0 c ty = match ty with
  | TyConstr (c', []) when c=c' -> true
  | _ -> false
let is_event_type t = is_type_constr0 "event" t 
let is_bool_type t = is_type_constr0 "bool" t 
let mk_type_fun ty_args ty_res = type_arrow (type_product ty_args) ty_res


let occur_check ~loc var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' ->
        if var == var' then raise(Type_circularity(loc, TyVar var, ty))
    | TyArrow (ty1,ty2) ->
        test ty1;
        test ty2
    | TyProduct ts ->
        List.iter test ts
    | TyConstr(_, args) ->
        List.iter test args in
  test ty

let rec unify ~loc ty1 ty2 =
  let val1 = real_type ty1
  and val2 = real_type ty2 in
  if val1 == val2 then () else
  match (val1, val2) with
  | TyVar v1, TyVar v2 when v1==v2 -> 
      ()
  | TyVar var, ty ->
      occur_check ~loc var ty;
      var.value <- Known ty
  | ty, TyVar var ->
      occur_check ~loc var ty;
      var.value <- Known ty
  | TyArrow(ty1, ty2), TyArrow(ty1', ty2') ->
      unify ~loc ty1 ty1';
      unify ~loc ty2 ty2'
  | TyProduct ts1, TyProduct ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 (unify ~loc) ts1 ts2
  | TyConstr (constr1, args1), TyConstr (constr2, args2)
    when constr1=constr2 && List.length args1 = List.length args2 ->
      List.iter2 (unify ~loc) args1 args2
  | _, _ ->
      raise (Type_conflict(loc,val1,val2))

let new_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let mk_type_var () = { stamp = new_stamp(); value = Unknown }
let new_type_var () = TyVar (mk_type_var ())

(* Generalisation / instanciation *)

let vars_of ty = (* Returns the list of type variables occuring in [t] *)
  let vars = ref [] in
  let rec scan t =
    match type_repr t with
    | TyVar var ->
       if not (List.memq var !vars) then vars := var :: !vars
    | TyArrow (t1, t2) ->
       scan t1;
       scan t2
    | TyProduct ts ->
       List.iter scan ts
    | TyConstr (_, args) ->
       List.iter scan args in
  scan ty;
  !vars

let generalize t =
  (* Turn a type [t] into a type scheme by extracting all type variables occuring in [t] *)
  (* This a simplified version of the classical [generalize] function in which all variables are taken as generic *)  
  { ts_params = vars_of t; ts_body = t }

let trivial_scheme ty =
  { ts_params = []; ts_body = ty }
  
let copy vars t = (* Make a copy a type, replacing all type variables occuring in [vars] *)
  let rec cp ty = 
    match type_repr ty with
    | TyVar var as ty ->
        begin try
          List.assq var vars
        with Not_found ->
            ty
        end
    | TyArrow (ty1, ty2) ->
        TyArrow (cp ty1, cp ty2)
    | TyProduct ts ->
        TyProduct (List.map cp ts)
    | TyConstr (c, args) ->
        TyConstr (c, List.map cp args) in
  cp t 

let type_instance ts =
  (* Take an instance of a type scheme, replacing all its generic variables by fresh ones *)
  let gvars = List.map (fun var -> (var, new_type_var())) ts.ts_params in
  copy gvars ts.ts_body

(* Printing *)

let rec pp_typ ?(abbrev=false) fmt t =
  let open Format in
  match real_type t with
  | TyConstr (c,[]) -> fprintf fmt "%s" c
  | TyConstr (c,[t']) -> fprintf fmt "%a %s" (pp_typ ~abbrev) t' c 
  | TyConstr (c,ts) -> fprintf fmt " %a %s" (Rfsm.Ext.List.pp_h (pp_typ ~abbrev)) ts c
  | TyProduct [t] -> (pp_typ ~abbrev) fmt t
  | TyProduct ts -> Rfsm.Ext.List.pp_h ~sep:"*" (pp_typ ~abbrev) fmt ts
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" (pp_typ ~abbrev) t1 (pp_typ ~abbrev) t2 
  | TyVar v -> fprintf fmt "_%d" v.stamp

let pp_var fmt tv = Format.fprintf fmt "_%d" tv.stamp (* TO FIX. Use 'a, 'b, ... instead *) 

let pp_typ_scheme fmt t = 
  let open Format in
  match t.ts_params with
  | [] ->
     fprintf fmt "@[<h>%a@]" (pp_typ ~abbrev:false) t.ts_body
  | _ ->
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Ext.List.pp_h ~sep:"," pp_var) t.ts_params (pp_typ ~abbrev:false) t.ts_body
