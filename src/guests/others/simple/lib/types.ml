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

module Location = Rfsm.Location

let print_full_types = ref false (* for debug only *)

type typ =
  | TyVar of typ var
  | TyArrow of typ * typ
  | TyProduct of typ list
  | TyConstr of string * typ list (** Name, args *)
  | TyRecord of string * (string * typ) list    (** Name, fields *)
  | TyArray of typ * int (**  Type of elements, size *)

and 'a var =
  { stamp: int;             (* for debug only *)
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_params: (typ var) list;
    ts_body: typ }

(* Type variables *)
  
let new_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let make_var () = { stamp=new_stamp (); value = Unknown }
let new_type_var () = TyVar (make_var ())

(* Builders *)

let no_type = TyProduct []

let type_arrow t1 t2 = TyArrow (t1, t2)
let type_arrow2 t1 t2 t3 = type_arrow t1 (type_arrow t2 t3)
let type_product = function
  | [t] -> t
  | ts -> TyProduct ts
let type_int sz = TyConstr ("int", [])
let type_array t sz = TyArray (t,sz)
let type_event () = TyConstr ("event", [])
let type_bool () = TyConstr ("bool", [])
let type_float () = TyConstr ( "float", [])
let type_char () = TyConstr ( "char", [])

let mk_type_constr0 c = TyConstr (c, [])
let is_type_constr0 c ty = match ty with
  | TyConstr (c', []) when c=c' -> true
  | _ -> false
let is_event_type t = is_type_constr0 "event" t 
let is_bool_type t = is_type_constr0 "bool" t 
let mk_type_fun ty_args ty_res = type_arrow (type_product ty_args) ty_res

let rec type_repr = function
  | TyVar ({value = Known ty1; _} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

(* Real type : path compression + unabbreviation *)

let rec real_type ty = 
  match type_repr ty with
  | TyVar { value=Known ty'; _} -> ty'
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyConstr (c, ts) -> TyConstr (c, List.map real_type ts)
  | TyRecord (name, fds) -> TyRecord (name, List.map (function (n,ty') -> n, real_type ty') fds)
  | ty -> ty

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
       List.iter scan args
    | TyRecord (_, fds) ->
       List.iter (function (_,t) -> scan t) fds 
    | TyArray (ty', sz) ->
       scan ty' in
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
        TyConstr (c, List.map cp args)
    | TyRecord (nm, fds) ->
       TyRecord(nm, List.map (function (n,t) -> n, cp t) fds) 
    | TyArray (ty', sz) ->
       TyArray (cp ty', sz) in
  cp t 

let type_instance ts =
  (* Take an instance of a type scheme, replacing all its generic variables by fresh ones *)
  let gvars = List.map (fun var -> (var, new_type_var())) ts.ts_params in
  copy gvars ts.ts_body

(* Type unification - the classical algorithm *)

exception Type_circularity of Location.t * typ * typ
exception Type_conflict of Location.t * typ * typ

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
       when constr1=constr2
            && List.length args1 = List.length args2 ->
     List.iter2 (unify ~loc) args1 args2
  | TyRecord (nm1,fds1), TyRecord (nm2,fds2) ->
     List.iter2
       (fun (n1,t1) (n2,t2) ->
         if n1 = n2
         then unify ~loc t1 t2
         else raise (Type_conflict(loc,val1,val2)))
       fds1 fds2
  | TyArray (arg1,sz1), TyArray (arg2, sz2) when sz1=sz2 ->
     unify ~loc arg1 arg2
  | _, _ ->
      raise (Type_conflict(loc,val1,val2))

and pp_var fmt v = Format.fprintf fmt "_%d" v.stamp (* TO FIX *) 

and occur_check ~loc var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' -> if var == var' then raise(Type_circularity(loc,TyVar var,ty))
    | TyArrow (ty1,ty2) -> test ty1; test ty2
    | TyProduct ts -> List.iter test ts
    | TyConstr(_, args) -> List.iter test args
    | _ -> ()
  in test ty

(* Printing *)

let rec pp_typ ?(abbrev=false) fmt t =
  let open Format in
  match real_type t with
  | TyVar v -> fprintf fmt "_%d" v.stamp
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" (pp_typ ~abbrev) t1 (pp_typ ~abbrev) t2 
  | TyProduct [t] -> if !print_full_types then fprintf fmt "(%a)" (pp_typ ~abbrev) t else (pp_typ ~abbrev) fmt t
  | TyProduct ts -> Rfsm.Ext.List.pp_h ~sep:"*" (pp_typ ~abbrev) fmt ts
  | TyConstr (c,[]) -> fprintf fmt "%s" c 
  | TyConstr (c,[t']) -> fprintf fmt "%a %s" (pp_typ ~abbrev) t' c 
  | TyConstr (c,ts) -> fprintf fmt " (%a) %s" (Rfsm.Ext.List.pp_h ~sep:"," (pp_typ ~abbrev)) ts c 
  | TyRecord (nm,fs) ->
     if abbrev
     then fprintf fmt "%s" nm
     else fprintf fmt "{%a}" (Rfsm.Ext.List.pp_h ~sep:"," (pp_rfield ~abbrev)) fs
  | TyArray (t',sz) -> fprintf fmt "%a array[%d]" (pp_typ ~abbrev) t' sz 

and pp_rfield ~abbrev fmt (n,ty) = Format.fprintf fmt "%s: %a" n (pp_typ ~abbrev) ty

let pp_typ_scheme fmt t = 
  let open Format in
  match t.ts_params with
  | [] ->
     fprintf fmt "@[<h>%a@]" (pp_typ ~abbrev:false) t.ts_body
  | _ ->
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Ext.List.pp_h ~sep:"," pp_var) t.ts_params (pp_typ ~abbrev:false) t.ts_body
