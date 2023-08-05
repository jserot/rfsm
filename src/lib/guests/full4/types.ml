module Location = Rfsm.Location

let print_full_types = ref true (* for debug only *)

type typ =
  | TyVar of typ var
  | TyArrow of typ * typ
  | TyProduct of typ list
  | TyConstr of string * typ list * siz (** name, args, size annotation *)
              (* For int's and arrays, [[s1]] gives size
                 For int's, [[s1;s2]] gives range *)
  | TyRecord of string * (string * typ) list    (** Name, fields *)

and siz = 
  | SzNone
  | SzVar of siz var
  | SzVal1 of size_value  (** For int size and array dimension *)
  | SzVal2 of size_value * size_value  (** For int range and 2D array dimensions *)

and size_value = 
  | SzConst of int
  | SzIndex of Rfsm.Ident.t  (* TOFIX : indexes should be replaced by constants by the elab phase *)

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

(* Type indexes *)

type index = int

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
let type_unsized_int () = TyConstr ("int", [], new_size_var ()) (* type [int] is really [int['a]] *)
let type_sized_int sz = TyConstr ("int", [], sz)
let type_ranged_int lo hi = TyConstr ("int", [], SzVal2 (lo,hi))

let type_unsized_array t = TyConstr ("array", [t], new_size_var ())
let type_sized_array t sz = TyConstr ("array", [t], SzVal1 sz)

let type_event () = TyConstr ("event", [], SzNone)
let type_bool () = TyConstr ("bool", [], SzNone)
let type_float () = TyConstr ( "float", [], SzNone)
let type_char () = TyConstr ( "char", [], SzNone)

let mk_type_constr0 c = TyConstr (c, [], SzNone)
let is_type_constr0 c ty = match ty with
  | TyConstr (c', [], _) when c=c' -> true
  | _ -> false
let mk_type_fun ty_args ty_res = type_arrow (type_product ty_args) ty_res

let is_index_type ty = is_type_constr0 "int" ty

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

let rec copy_type tvbs svbs ty =
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

let type_instance ty_sch =
  match ty_sch.ts_tparams, ty_sch.ts_sparams with
  | [], [] -> ty_sch.ts_body
  | tparams, sparams ->
      let unknown_ts = List.map (fun var -> (var, new_type_var())) tparams in
      let unknown_ss = List.map (fun var -> (var, new_size_var())) sparams in
      copy_type unknown_ts unknown_ss ty_sch.ts_body

let size_of_type t =
  match real_type t with
  | TyConstr (_, _, SzVal1 (SzConst s)) -> [s]
  | TyConstr (_, _, SzVal2 ((SzConst s1,SzConst s2))) -> [s1;s2]
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
     begin
       try
         List.iter2 (unify ~loc) args1 args2;
         unify_size ~loc constr1 (val1,val2) sz1 sz2
      with
        Invalid_argument _ -> raise (Type_conflict(loc,val1,val2))
     end
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

and unify_size ~loc c (ty1,ty2) sz1 sz2 =
  let s1 = real_size sz1
  and s2 = real_size sz2 in
  if s1 == s2 then ()
  else match s1, s2 with
    | SzVar var1, SzVar var2 when var1 == var2 -> ()
    | SzVar var, sz ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | SzVal1 v1, SzVal1 v2 ->
       unify_size_value ~loc c (ty1,ty2) v1 v2
    | SzVal2 (v11,v12), SzVal2 (v21,v22) ->
       unify_size_value ~loc c (ty1,ty2) v11 v21;
       unify_size_value ~loc c (ty1,ty2) v12 v22
    (* | SzVal1 s1, SzVal2 (lo2,hi2), "int" when lo2 >= 0 && lo2 < Rfsm.Misc.pow2 s1 && hi2 >= 0 && hi2 < Rfsm.Misc.pow2 s1 -> ()  
     * | SzVal2 (lo2,hi2), SzVal1 s1, "int" when lo2 >= 0 && lo2 < Rfsm.Misc.pow2 s1 && hi2 >= 0 && hi2 < Rfsm.Misc.pow2 s1 -> ()  
     * (\* The very special case. We unify, [int<n>] with [int<lo:hi>] iff 
     *     - [n] and [lo] and [hi] are constants. 
     *     - the value of [lo] (resp. [hi]) is in [[0,...,2^n-1]] (assuming here unsigned ints) *\)  *)
    | _, _ ->
        raise (Type_conflict(loc,ty1,ty2))

and unify_size_value ~loc c (ty1,ty2) sv1 sv2 =
  match sv1, sv2 with
  | SzConst c1, SzConst c2 when c1 = c2 -> ()
  | SzIndex i1, SzIndex i2 when i1 = i2 -> ()  (* TO FIX : this should not occur after elaboration *)
  | SzIndex _, SzConst _ -> ()  (* TO FIX: this too permissive - and  should not occur after elaboration, anyway *)
  | SzConst _, SzIndex _ -> ()  (* TO FIX: this too permissive - and  should not occur after elaboration, anyway *)
  | _, _ -> raise (Type_conflict(loc,ty1,ty2))
  
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

let rec pp_typ ~abbrev fmt t =
  let open Format in
  match real_type t with
  | TyVar v -> fprintf fmt "_%d" v.stamp
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" (pp_typ ~abbrev) t1 (pp_typ ~abbrev) t2 
  | TyProduct [] -> fprintf fmt "<no_type>" 
  | TyProduct [t] -> if !print_full_types then fprintf fmt "(%a)" (pp_typ ~abbrev) t else (pp_typ ~abbrev) fmt t
  | TyProduct ts -> Rfsm.Misc.pp_list_h ~sep:"*" (pp_typ ~abbrev) fmt ts
  | TyConstr (c,[],sz) -> fprintf fmt "%s%a" c (pp_size c) sz
  | TyConstr (c,[t'],sz) -> fprintf fmt "%a %s%a" (pp_typ ~abbrev) t' c (pp_size c) sz
  | TyConstr (c,ts,sz) -> fprintf fmt " (%a) %s%a" (Rfsm.Misc.pp_list_h ~sep:"," (pp_typ ~abbrev)) ts c (pp_size c) sz
  | TyRecord (nm,fs) ->
     if abbrev
     then fprintf fmt "%s" nm
     else fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp_rfield ~abbrev)) fs

and pp_rfield ~abbrev fmt (n,ty) = Format.fprintf fmt "%s: %a" n (pp_typ ~abbrev) ty

(* and pp_sizes c fmt szs = 
 *   match c, szs with
 *   | _, [] -> if !print_full_types then Format.fprintf fmt "<none>" else ()
 *   | "array", [sz] -> Format.fprintf fmt "[%a]" pp_size sz
 *   | "int", [sz] -> Format.fprintf fmt "<%a>" pp_size sz
 *   | _, [sz] -> Format.fprintf fmt "<%a>" pp_size sz
 *   | "int", [lo;hi] -> Format.fprintf fmt "<%a:%a>" pp_size lo pp_size hi
 *   | "array", [lo;hi] -> Format.fprintf fmt "[%a,%a]" pp_size lo pp_size hi
 *   | _, [lo;hi] -> Format.fprintf fmt "<%a,%a>" pp_size lo pp_size hi
 *   | _, _ -> Format.fprintf fmt "<???>" *)

and pp_size c fmt sz = 
  let pp_siz_var fmt v = Format.fprintf fmt "_%d" v.stamp in (* TO FIX ? *) 
  let pp_siz_val fmt v = match v with
    | SzConst s -> Format.fprintf fmt "%d" s
    | SzIndex i -> Format.fprintf fmt "%a" Rfsm.Ident.pp i in
  match c, real_size sz with
  | _, SzNone -> ()
  | "array", SzVar v -> Format.fprintf fmt "[%a]" pp_siz_var v
  | _, SzVar v -> Format.fprintf fmt "<%a>" pp_siz_var v
  | "array", SzVal1 v -> Format.fprintf fmt "[%a]" pp_siz_val v
  | _ , SzVal1 v -> Format.fprintf fmt "<%a>" pp_siz_val v
  | "array", SzVal2 (v1,v2) -> Format.fprintf fmt "[%a][%a]" pp_siz_val v1 pp_siz_val v2
  | "int" , SzVal2 (v1,v2) -> Format.fprintf fmt "<%a:%a>" pp_siz_val v1 pp_siz_val v2
  | _ , SzVal2 (v1,v2) -> Format.fprintf fmt "<%a,%a>" pp_siz_val v1 pp_siz_val v2

let pp_typ_scheme fmt t =
  let open Format in
  match t.ts_sparams with
  | [] ->
     fprintf fmt "@[<h>%a@]" (pp_typ ~abbrev:false) t.ts_body
  | _ ->
     let pp_var fmt v = Format.fprintf fmt "_%d" v.stamp in (* TO FIX ? *)  
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Misc.pp_list_h ~sep:"," pp_var) t.ts_sparams (pp_typ ~abbrev:false) t.ts_body
