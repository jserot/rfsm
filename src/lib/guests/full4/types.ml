module Location = Rfsm.Location

let print_full_types = ref true (* for debug only *)

type typ =
  | TyVar of typ var
  | TyArrow of typ * typ
  | TyProduct of typ list
  | TyConstr of string * typ list * siz list (** name, args, size annotation *)
              (* For int's and arrays, [[s1]] gives size
                 For int's, [[s1;s2]] gives range *)
  | TyRecord of string * (string * typ) list    (** Name, fields *)

and siz = 
  | SzVar of siz var
  | SzIndex of Rfsm.Ident.t
  | SzConst of int

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
let type_unsized_int () = TyConstr ("int", [], [new_size_var ()])
let type_sized_int sz = TyConstr ("int", [], [sz])
let type_ranged_int lo hi = TyConstr ("int", [], [lo;hi])

let type_unsized_array t = TyConstr ("array", [t], [new_size_var ()])
let type_sized_array t sz = TyConstr ("array", [t], [sz])

let type_event () = TyConstr ("event", [], [])
let type_bool () = TyConstr ("bool", [], [])
let type_float () = TyConstr ( "float", [], [])
let type_char () = TyConstr ( "char", [], [])

let mk_type_constr0 c = TyConstr (c, [], [])
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
  | TyConstr (c, ts, szs) -> TyConstr (c, List.map real_type ts, List.map real_size szs)
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
    | TyConstr (c, args, szs) ->
       TyConstr (c, List.map copy args, List.map (copy_size ty svbs) szs)
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

exception Size_of_type

let size_of_type t =
  let int_of sz = match sz with
    | SzConst c -> c
    | _ -> raise Size_of_type in
  match real_type t with
  | TyConstr (_, _, szs) ->
     begin 
       try List.map int_of szs
       with Size_of_type -> [] 
     end
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
  | TyConstr (constr1, args1, szs1), TyConstr (constr2, args2, szs2)
       when constr1=constr2 && List.length args1 = List.length args2 ->
     begin
       try
         List.iter2 (unify ~loc) args1 args2;
         List.iter2 (unify_size ~loc (val1,val2)) szs1 szs2
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

and unify_size ~loc (ty1,ty2) sz1 sz2 =
  let s1 = real_size sz1
  and s2 = real_size sz2 in
  if s1 == s2 then ()
  else match (s1,s2) with
    | SzVar var1, SzVar var2 when var1 == var2 ->
        ()
    | SzVar var, sz ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | SzConst c1, SzConst c2 when c1 = c2 ->
       ()
    (* Note: SzIndexes have normally been eliminated at this level *)
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

let rec pp_typ ~abbrev fmt t =
  let open Format in
  match real_type t with
  | TyVar v -> fprintf fmt "_%d" v.stamp
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" (pp_typ ~abbrev) t1 (pp_typ ~abbrev) t2 
  | TyProduct [] -> fprintf fmt "<no_type>" 
  | TyProduct [t] -> if !print_full_types then fprintf fmt "(%a)" (pp_typ ~abbrev) t else (pp_typ ~abbrev) fmt t
  | TyProduct ts -> Rfsm.Misc.pp_list_h ~sep:"*" (pp_typ ~abbrev) fmt ts
  | TyConstr (c,[],szs) -> fprintf fmt "%s%a" c (pp_sizes c) szs
  | TyConstr (c,[t'],szs) -> fprintf fmt "%a %s%a" (pp_typ ~abbrev) t' c (pp_sizes c) szs
  | TyConstr (c,ts,szs) -> fprintf fmt " (%a) %s%a" (Rfsm.Misc.pp_list_h ~sep:"," (pp_typ ~abbrev)) ts c (pp_sizes c) szs
  | TyRecord (nm,fs) ->
     if abbrev
     then fprintf fmt "%s" nm
     else fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp_rfield ~abbrev)) fs

and pp_rfield ~abbrev fmt (n,ty) = Format.fprintf fmt "%s: %a" n (pp_typ ~abbrev) ty

and pp_sizes c fmt szs = 
  match c, szs with
  | _, [] -> if !print_full_types then Format.fprintf fmt "<none>" else ()
  | "array", [sz] -> Format.fprintf fmt "[%a]" pp_size sz
  | "int", [sz] -> Format.fprintf fmt "<%a>" pp_size sz
  | _, [sz] -> Format.fprintf fmt "<%a>" pp_size sz
  | "int", [lo;hi] -> Format.fprintf fmt "<%a:%a>" pp_size lo pp_size hi
  | "array", [lo;hi] -> Format.fprintf fmt "[%a,%a]" pp_size lo pp_size hi
  | _, [lo;hi] -> Format.fprintf fmt "<%a,%a>" pp_size lo pp_size hi
  | _, _ -> Format.fprintf fmt "<???>"

and pp_size fmt sz = 
  match real_size sz with
  | SzVar v -> Format.fprintf fmt "_%d" v.stamp (* TO FIX ? *) 
  | SzIndex i -> Format.fprintf fmt "%a" Rfsm.Ident.pp i
  | SzConst c -> Format.fprintf fmt "%d" c

let pp_typ_scheme fmt t =
  let open Format in
  match t.ts_sparams with
  | [] ->
     fprintf fmt "@[<h>%a@]" (pp_typ ~abbrev:false) t.ts_body
  | _ ->
     let pp_var fmt v = Format.fprintf fmt "_%d" v.stamp in (* TO FIX ? *)  
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Misc.pp_list_h ~sep:"," pp_var) t.ts_sparams (pp_typ ~abbrev:false) t.ts_body
