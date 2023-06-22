module Location = Rfsm.Location

module VarSet = Set.Make(struct type t = string let compare = Stdlib.compare end)
                 
let print_full_types = ref false (* for debug only *)

(* Type indexes *)

module Index = struct 

  type t =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * t * t

  type env = (string * int) list

  type op = int -> int -> int

  let ops = [
    "+", (+);
    "-", (-);
    "*", ( * ); 
    "/", ( / );
    "mod", ( mod ) 
    ]
  
  exception Illegal_op of string
  exception Illegal_type_index of t
  exception Unbound_type_index of string

  let lookup op = 
    try List.assoc op ops
    with Not_found -> raise (Illegal_op op)

  let rec subst phi i = match i with 
        TiConst _ -> i
      | TiVar v ->
         begin
           try TiConst (List.assoc v phi)
           with Not_found -> raise (Unbound_type_index v)
         end
      | TiBinop (op,e1,e2) ->
         begin match subst phi e1, subst phi e2 with
         | TiConst c1, TiConst c2 -> TiConst ((lookup op) c1 c2)
         | i1, i2 -> TiBinop (op, i1, i2)
         end 

  let rec vars_of = function
    | TiConst _ -> VarSet.empty
    | TiVar v -> VarSet.singleton v
    | TiBinop (_,e1,e2) -> VarSet.union (vars_of e1) (vars_of e2)
  
  let rec pp fmt i = 
    let open Format in
    match i with
    | TiConst c -> fprintf fmt "%d" c
    | TiVar v -> fprintf fmt "%s" v
    | TiBinop (op,e1,e2) -> fprintf fmt "%a%s%a" pp e1 op pp e2 (* TODO: add parens *)
                
end

type typ =
  | TyVar of typ var
  | TyArrow of typ * typ
  | TyProduct of typ list
  | TyConstr of string * typ list * siz (** name, args, size annotation *)
              (* For ints: bit width or range, for arrays: dimension *)
  | TyRecord of string * (string * typ) list    (** Name, fields *)

and siz =
  | SzNone 
  | SzVar of siz var   
  | SzExpr1 of Index.t                  (* For ints: bit width, for arrays: dimension *)
  | SzExpr2 of Index.t * Index.t        (* For ints: range, for arrays: dimensions *)

and name =
  | NmLit of string
  | NmVar of name var   

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
let new_name_var () = NmVar (make_var ())

(* Builders *)

let no_type = TyProduct []

let type_arrow t1 t2 = TyArrow (t1, t2)
let type_arrow2 t1 t2 t3 = type_arrow t1 (type_arrow t2 t3)
let type_product = function
  | [t] -> t
  | ts -> TyProduct ts
            
(* let type_constr c args szs =
 *   match szs with
 *   | [] -> TyConstr (c, args, new_size_var ())
 *   | [w] -> TyConstr (c, args, SzExpr1 (TiConst w))
 *   | [lo;hi] -> TyConstr (c, args, SzExpr2 (TiConst lo, TiConst hi))
 *   | _ -> Rfsm.Misc.fatal_error "Full.Types.type_sized" *)

let type_int sz = TyConstr ("int", [], sz)
let type_unsized_int () = TyConstr ("int", [], new_size_var ())
let type_sized_int sz = TyConstr ("int", [], SzExpr1 (TiConst sz))
let type_ranged_int lo hi = TyConstr ("int", [], SzExpr2 (TiConst lo, TiConst hi))

let type_array t sz = TyConstr ("array", [t], sz)
let type_unsized_array t = TyConstr ("array", [t], new_size_var ())
let type_sized_array t sz = TyConstr ("array", [t], SzExpr1 (TiConst sz))
(* let type_sized_array2 t sz1 sz2 = TyConstr ("array", [t], SzExpr2 (TiConst sz1, TiConst sz2)) *)

let type_event () = TyConstr ("event", [], SzNone)
let type_bool () = TyConstr ("bool", [], SzNone)
let type_float () = TyConstr ( "float", [], SzNone)

let mk_type_constr0 c = TyConstr (c, [], SzNone)
let is_type_constr0 c ty = match ty with
  | TyConstr (c', [], _) when c=c' -> true
  | _ -> false
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

let rec name_repr = function
  | NmVar ({value = Known nm1; _} as var) ->
      let nm = name_repr nm1 in
      var.value <- Known nm;
      nm
  | nm -> nm

(* Real type : path compression + unabbreviation *)

let rec real_type ty = 
  match type_repr ty with
  | TyVar { value=Known ty'; _} -> ty'
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyConstr (c, ts, sz) -> TyConstr (c, List.map real_type ts, real_size sz)
  | TyRecord (name, fds) -> TyRecord (real_name name, List.map (function (n,ty') -> n, real_type ty') fds)
  | ty -> ty

and real_size sz = 
  match size_repr sz with
  | SzVar { value=Known sz'; _} -> sz'
  | sz -> sz

and real_name nm = nm
  (* match name_repr nm with
   * | NmVar { value=Known nm'; _} -> nm'
   * | nm -> nm *)

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
       when constr1=constr2
            && List.length args1 = List.length args2 ->
     List.iter2 (unify ~loc) args1 args2;
     unify_size ~loc (val1,val2) sz1 sz2
  | TyRecord (nm1,fds1), TyRecord (nm2,fds2) ->
     List.iter2
       (fun (n1,t1) (n2,t2) ->
         if n1 = n2
         then unify ~loc t1 t2
         else raise (Type_conflict(loc,val1,val2)))
       fds1 fds2
     (* unify_name ~loc (val1,val2) nm1 nm2 *)
  | _, _ ->
      raise (Type_conflict(loc,val1,val2))

and unify_size ~loc (ty1,ty2) sz1 sz2 =
  let val1 = real_size sz1
  and val2 = real_size sz2 in
  if val1 == val2 then
    ()
  else
  match (val1, val2) with
    | SzNone, SzNone ->
       ()
    | SzVar var1, SzVar var2 when var1 == var2 ->  (* This is hack *)
        ()
    | SzVar var, sz ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size ~loc (ty1,ty2) var sz;
        var.value <- Known sz
    | SzExpr1 (TiConst w1), SzExpr1 (TiConst w2) when w1 = w2 ->
        ()
    | SzExpr2 (TiConst lo1, TiConst hi1), SzExpr2 (TiConst lo2, TiConst hi2) when lo1 = lo2 && hi1 = hi2 ->
        ()
    | _, _ ->
        raise (Type_conflict(loc,ty1,ty2))


(* and unify_index (ty1,ty2) idx1 idx2 =
 *   match idx1, idx2 with 
 *   | TiConst n1, TiConst n2 when n1<>n2 -> raise (TypeConflict(ty1, ty2))
 *   | _, _ -> ()  (\* TO BE FIXED ! *\) *)

(* and unify_name ~loc (ty1,ty2) nm1 nm2 =
 *   let val1 = real_name nm1
 *   and val2 = real_name nm2 in
 *   if val1 == val2 then
 *     ()
 *   else
 *   match (val1, val2) with
 *     | NmLit s1, NmLit s2 when s1 = s2 -> ()
 *     | NmVar var1, NmVar var2 when var1 == var2 -> () (\* This is hack *\)
 *     | NmVar var, nm -> var.value <- Known nm
 *     | nm, NmVar var -> var.value <- Known nm
 *     | _, _ ->
 *         raise (Type_conflict(loc,ty1,ty2)) *)

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

let ivars_of = function
  | TyConstr (_, _, SzExpr1 sz) -> VarSet.elements (Index.vars_of sz)
  | TyConstr (_, _, SzExpr2 (lo,hi)) -> VarSet.elements (VarSet.union (Index.vars_of lo) (Index.vars_of hi))
  | _ -> []

(* Index manipulation *)
       
let subst_indexes phi ty =
  match ty with
    | TyConstr (c, args, SzExpr1 sz) -> TyConstr (c, args, SzExpr1 (Index.subst phi sz))
    | TyConstr (c, args, SzExpr2 (lo,hi)) -> TyConstr (c, args, SzExpr2 (Index.subst phi hi, Index.subst phi lo))
    | _ -> ty


(* Checking *)

(* let rec type_equal ~strict t1 t2 =
 *   match real_type t1, real_type t2 with
 *   | TyBool, TyBool -> true
 *   | TyEvent, TyEvent -> true
 *   | TyInt sz1, TyInt sz2 -> size_equal ~strict:strict sz1 sz2
 *   | TyFloat, TyFloat -> true
 *   | TyChar, TyChar -> true
 *   | TyEnum (nm1,cs1), TyEnum (nm2,cs2) ->
 *      name_equal ~strict nm1 nm2 && 
 *      (if strict then List.sort compare cs1 = List.sort compare cs2
 *      else List.for_all (function c -> List.mem c cs1) cs2)
 *         (\* so that, for ex, [type_equal ~strict:false {On,Off} {On} = true] *\)
 *   | TyVar { stamp=s1; value=Unknown }, TyVar { stamp=s2; value=Unknown } -> s1 = s2
 *   | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
 *       type_equal ~strict ty1 ty2 && type_equal ~strict ty1' ty2'
 *   | TyProduct ts, TyProduct ts' when List.length ts = List.length ts'->
 *       List.for_all2 (type_equal ~strict) ts ts'
 *   | TyArray (sz1, ty1), TyArray (sz2, ty2) -> 
 *      sz1 = sz2 && type_equal ~strict ty1 ty2
 *   | TyRecord (nm1, fds1), TyRecord (nm2, fds2) ->
 *      name_equal ~strict nm1 nm2 && List.for_all2 (fun (n1,t1) (n2,t2) -> type_equal ~strict t1 t2) fds1 fds2
 *   | _, _ -> false
 * 
 * and size_equal ~strict s1 s2 =
 *   match real_size s1, real_size s2 with
 *   | SzExpr1 w1, SzExpr1 w2 -> w1 = w2
 *   | SzExpr2 (lo1,hi1), SzExpr2 (lo2,hi2) -> lo1 = lo2 && hi1 = hi2
 *   | SzVar v1, SzVar v2 -> v1 == v2
 *   | _, _ -> false
 * 
 * and name_equal ~strict nm1 nm2 =
 *   match real_name nm1, real_name nm2 with
 *   | NmLit s1, NmLit s2 -> s1 = s2
 *   | NmVar v1, NmVar v2 -> v1 == v2
 *   | _, _ -> false *)
    
(* Accessors *)
                 
(* let enums_of ty = match ty with
 *   | TyEnum (_,cs) -> List.map (function c -> c, ty) cs
 *   | _ -> []
 * 
 * let size_of ty = match ty with
 *   | TyArray (TiConst sz, _) -> sz
 *   | TyProduct ts -> List.length ts
 *   | _ -> 0
 * 
 * let subtype_of = function
 *   | TyArray (_,t) -> t
 *   | _ -> Misc.fatal_error "Types.subtype_of"
 *        
 * let is_lit_name nm = match real_name nm with
 *   | NmLit _ -> true
 *   | _ -> false *)
       
(* Printing *)

let rec pp_typ fmt t =
  let open Format in
  match real_type t with
  | TyVar v -> fprintf fmt "_%d" v.stamp
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" pp_typ t1 pp_typ t2 
  | TyProduct [t] -> if !print_full_types then fprintf fmt "(%a)" pp_typ t else pp_typ fmt t
  | TyProduct ts -> Rfsm.Misc.pp_list_h ~sep:"*" pp_typ fmt ts
  | TyConstr (c,[],sz) -> fprintf fmt "%s%a" c (pp_siz c) sz
  | TyConstr (c,[t'],sz) -> fprintf fmt "%a %s%a" pp_typ t' c (pp_siz c) sz
  | TyConstr (c,ts,sz) -> fprintf fmt " (%a) %s%a" (Rfsm.Misc.pp_list_h ~sep:"," pp_typ) ts c (pp_siz c) sz
  | TyRecord (nm,fs) -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," pp_rfield) fs

and pp_siz c fmt sz = 
  match c, size_repr sz with
  | _, SzNone -> if !print_full_types then Format.fprintf fmt "<none>" else ()
  | "int", SzVar v -> if !print_full_types then Format.fprintf fmt "<%a>" pp_var v else ()
  | "array", SzVar v -> if !print_full_types then Format.fprintf fmt "[%a]" pp_var v else Format.fprintf fmt "[]"
  | _, SzVar v -> Format.fprintf fmt "%a" pp_var v
  | "array", SzExpr1 sz -> Format.fprintf fmt "[%a]" Index.pp sz
  | _, SzExpr1 sz -> Format.fprintf fmt "<%a>" Index.pp sz
  | "int", SzExpr2 (lo,hi) -> Format.fprintf fmt "<%a:%a>" Index.pp lo Index.pp hi
  | "array", SzExpr2 (lo,hi) -> Format.fprintf fmt "[%a,%a]" Index.pp lo Index.pp hi
  | _, SzExpr2 (lo,hi) -> Format.fprintf fmt "<%a,%a>" Index.pp lo Index.pp hi

and pp_var fmt v = Format.fprintf fmt "_%d" v.stamp (* TO FIX *) 

and pp_rfield fmt (n,ty) = Format.fprintf fmt "%s: %a" n pp_typ ty

let pp_typ_scheme fmt t = (* TODO: add size parameters *)
  let open Format in
  match t.ts_sparams with
  | [] ->
     fprintf fmt "@[<h>%a@]" pp_typ t.ts_body
  | _ ->
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Misc.pp_list_h ~sep:"," pp_var) t.ts_sparams pp_typ t.ts_body
