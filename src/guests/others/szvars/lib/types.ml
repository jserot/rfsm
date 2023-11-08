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

let print_full_types = ref true (* for debug only *)

type typ =
  | TyVar of typ var
  | TyArrow of typ * typ
  | TyProduct of typ list
  | TyConstr of string * typ list * siz (** name, args, size annotation(s) *)
  | TyRecord of string * (string * typ) list    (** Name, fields *)

and siz =
  | SzVar of siz var
  | SzNone
  | Sz1 of int (* For int's and arrays: size *)
  | Sz2 of int * int (* For int's: range *)

and 'a var =
  { stamp: int;             (* for debug only *)
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_tparams: (typ var) list;
    ts_sparams: (siz var) list;
    ts_body: typ }

(* Type variables *)
  
let new_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let make_var () = { stamp=new_stamp (); value = Unknown }
let new_type_var () = TyVar (make_var ())
let new_size_var () = SzVar (make_var ())

(* Builders *)

let no_type = TyProduct []

let type_arrow t1 t2 = TyArrow (t1, t2)
let type_arrow2 t1 t2 t3 = type_arrow t1 (type_arrow t2 t3)
let type_product = function
  | [t] -> t
  | ts -> TyProduct ts
let type_int sz = TyConstr ("int", [], sz)
let type_unsized_int () = TyConstr ("int", [], new_size_var ())
let type_sized_int sz = TyConstr ("int", [], Sz1 sz)
let type_ranged_int lo hi = TyConstr ("int", [], Sz2 (lo,hi))

let type_unsized_array t = TyConstr ("array", [t], new_size_var ())
let type_sized_array t sz = TyConstr ("array", [t], Sz1 sz)

let type_event () = TyConstr ("event", [], SzNone)
let type_bool () = TyConstr ("bool", [], SzNone)
let type_float () = TyConstr ( "float", [], SzNone)
let type_char () = TyConstr ( "char", [], SzNone)

let mk_type_constr0 c = TyConstr (c, [], SzNone)
let is_type_constr0 c ty = match ty with
  | TyConstr (c', [], _) when c=c' -> true
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

let rec size_repr = function
  | SzVar ({value = Known sz1; _} as var) ->
      let sz = size_repr sz1 in
      var.value <- Known sz;
      sz
  | sz -> sz

(* Generalisation / instanciation *)

let vars_of ty = (* Returns the lists of type and size variables occuring in [t] *)
    let tvars, svars = ref [], ref [] in
    let rec scan_ty t =
      match type_repr t with
      | TyVar var ->
          if not (List.memq var !tvars) then tvars := var :: !tvars
      | TyArrow (t1, t2) ->
          scan_ty t1;
          scan_ty t2
      | TyProduct ts ->
          List.iter scan_ty ts
      | TyConstr (_, args, sz) ->
          List.iter scan_ty args;
          scan_sz sz
      | TyRecord (_, fs) ->
         List.iter (fun (_, t) -> scan_ty t) fs
    and scan_sz sz = 
      match size_repr sz with 
      | SzVar var -> 
          if not (List.memq var !svars) then svars := var :: !svars
      | _ -> () in
    scan_ty ty;
    (!tvars, !svars)


let generalize t =
  (* Turn a type [t] into a type scheme by extracting all type and size variables occuring in [t] *)
  (* This a simplified version of the classical [generalize] function in which all variables are taken as generic *)  
  let tvars, svars = vars_of t in 
  { ts_tparams = tvars; ts_sparams = svars; ts_body = t }

let trivial_scheme ty =
  { ts_tparams = []; ts_sparams = []; ts_body = ty }

let rec copy tvbs svbs ty =
  (* Make a copy a type, replacing all type (resp. size) variables occuring in [tvbs] (resp. svbs) by fresh copies *)
  let rec copy ty = 
    match type_repr ty with
    | TyVar var as ty ->
        begin try
          List.assq var tvbs
        with Not_found ->
            ty
        end
    | TyArrow (ty1, ty2) ->
       TyArrow (copy ty1, copy ty2)
    | TyProduct ts ->
       TyProduct (List.map copy ts)
    | TyConstr (c, args, sz) ->
       TyConstr (c, List.map copy args, copy_size ty svbs sz)
    | TyRecord (nm, fds) ->
       TyRecord(nm, List.map (function (n,t) -> n, copy t) fds) in
  copy ty

and copy_size ty svbs sz =
  match size_repr sz with
  | SzVar var as sz ->
      begin try
        List.assq var svbs 
      with Not_found ->
        sz
      end
  | sz -> sz

let type_instance ts =
  (* Take an instance of a type scheme, replacing all its generic variables by fresh ones *)
  let tvars = List.map (fun var -> (var, new_type_var())) ts.ts_tparams in
  let svars = List.map (fun var -> (var, new_size_var())) ts.ts_sparams in
  copy tvars svars ts.ts_body

(* Real type : path compression + unabbreviation *)

let rec real_type ty = 
  match type_repr ty with
  | TyVar { value=Known ty'; _} -> ty'
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyConstr (c, ts, sz) -> TyConstr (c, List.map real_type ts, real_size sz)
  | TyRecord (name, fds) -> TyRecord (name, List.map (function (n,ty') -> n, real_type ty') fds)
  | ty -> ty

and real_size sz = 
  match size_repr sz with
  | SzVar { value=Known sz'; _} -> sz'
  | sz -> sz

let size_of_type t =
  match real_type t with
  | TyConstr (_, _, Sz1 s) -> [s]
  | TyConstr (_, _, Sz2 (s1,s2)) -> [s1;s2]
  | _ -> []

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
  | TyConstr (constr1, args1, sz1), TyConstr (constr2, args2, sz2)
       when constr1=constr2 && List.length args1 = List.length args2 ->
     List.iter2 (unify ~loc) args1 args2;
     unify_size ~loc (val1,val2) sz1 sz2
  | TyRecord (nm1,fds1), TyRecord (nm2,fds2) ->
     (* TODO: what do we do with nm1 and nm2 ? *)
     List.iter2
       (fun (n1,t1) (n2,t2) ->
         if n1 = n2
         then unify ~loc t1 t2
         else raise (Type_conflict(loc,val1,val2)))
       fds1 fds2
  | _, _ ->
      raise (Type_conflict(loc,val1,val2))

and unify_size ~loc (ty1,ty2) sz1 sz2 =
  let s1 = real_size sz1
  and s2 = real_size sz2 in
  if s1 == s2 then ()
  else match (s1,s2) with
    | SzNone, SzNone ->
       ()
    | SzVar var1, SzVar var2 when var1 == var2 ->
        ()
    | SzVar var, sz ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | Sz1 v1, Sz1 v2 when v1=v2 -> 
       ()
    | Sz2 (v11,v12), Sz2 (v21,v22) when v11=v21 && v12=v22 -> 
       ()
    | _, _ ->
        raise (Type_conflict(loc,ty1,ty2))
  
and occur_check ~loc var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' -> if var == var' then raise(Type_circularity(loc,TyVar var,ty))
    | TyArrow (ty1,ty2) -> test ty1; test ty2
    | TyProduct ts -> List.iter test ts
    | TyConstr(_, args, sz) -> List.iter test args
    | _ -> ()
  in test ty

and occur_check_size ~loc (ty1,ty2) var sz =
  let test s =
    match size_repr s with
    | SzVar var' ->
        if var == var' then raise(Type_circularity(loc,ty1,ty2))
    | _ ->
        ()
  in test sz

(* Printing *)

let pp_var fmt v = Format.fprintf fmt "_%d" v.stamp (* TO FIX *) 

let rec pp_typ ?(abbrev=false) fmt t =
  let open Format in
  match real_type t with
  | TyVar v -> fprintf fmt "_%d" v.stamp
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" (pp_typ ~abbrev) t1 (pp_typ ~abbrev) t2 
  | TyProduct [t] -> if !print_full_types then fprintf fmt "(%a)" (pp_typ ~abbrev) t else (pp_typ ~abbrev) fmt t
  | TyProduct ts -> Rfsm.Ext.List.pp_h ~sep:"*" (pp_typ ~abbrev) fmt ts
  | TyConstr (c,[],szs) -> fprintf fmt "%s%a" c (pp_siz c) szs
  | TyConstr (c,[t'],szs) -> fprintf fmt "%a %s%a" (pp_typ ~abbrev) t' c (pp_siz c) szs
  | TyConstr (c,ts,szs) -> fprintf fmt " (%a) %s%a" (Rfsm.Ext.List.pp_h ~sep:"," (pp_typ ~abbrev)) ts c (pp_siz c) szs
  | TyRecord (nm,fs) ->
     if abbrev
     then fprintf fmt "%s" nm
     else fprintf fmt "{%a}" (Rfsm.Ext.List.pp_h ~sep:"," (pp_rfield ~abbrev)) fs

and pp_rfield ~abbrev fmt (n,ty) = Format.fprintf fmt "%s: %a" n (pp_typ ~abbrev) ty

and pp_siz c fmt sz = 
  match c, size_repr sz with
  | _, SzNone -> if !print_full_types then Format.fprintf fmt "<none>" else ()
  | "int", SzVar v -> if !print_full_types then Format.fprintf fmt "<%a>" pp_var v else ()
  | "array", SzVar v -> if !print_full_types then Format.fprintf fmt "[%a]" pp_var v else Format.fprintf fmt "[]"
  | _, SzVar v -> Format.fprintf fmt "%a" pp_var v
  | "array", Sz1 sz -> Format.fprintf fmt "[%d]" sz
  | _, Sz1 sz -> Format.fprintf fmt "<%d>" sz
  | "int", Sz2 (lo,hi) -> Format.fprintf fmt "<%d:%d>" lo hi
  | "array", Sz2 (lo,hi) -> Format.fprintf fmt "[%d,%d]" lo hi
  | c, Sz2 (lo,hi) -> Format.fprintf fmt "<%d,%d>" lo hi

let pp_typ_scheme fmt t = (* TODO: add size params ! *)
  let open Format in
  match t.ts_tparams with
  | [] ->
     fprintf fmt "@[<h>%a@]" (pp_typ ~abbrev:false) t.ts_body
  | _ ->
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Ext.List.pp_h ~sep:"," pp_var) t.ts_tparams (pp_typ ~abbrev:false) t.ts_body
