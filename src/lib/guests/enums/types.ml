module Location = Rfsm.Location
                
type typ =
   | TyVar of ty_var
   | TyArrow of typ * typ
   | TyProduct of typ list         
   | TyConstr of string * typ list * int option (** name, args, size annotation *)

and ty_var =
  { stamp: int; (* for debug *)
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_params: ty_var list;
    ts_body: typ }

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
let type_constr c ts sz = TyConstr(c, ts, sz)
let type_pair t1 t2 = TyProduct [t1;t2]
let type_product = function
  | [t] -> t
  | ts -> TyProduct ts
let type_int () = type_constr "int" [] None
let type_bool () = type_constr "bool" [] None

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
    | TyConstr(_, args, _) ->
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
  | TyConstr (constr1, args1, sz1), TyConstr (constr2, args2, sz2)
    when constr1=constr2 && List.length args1 = List.length args2 && sz1 = sz2 ->
      List.iter2 (unify ~loc) args1 args2
  | _, _ ->
      raise (Type_conflict(loc,val1,val2))

let new_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let mk_type_var () = { stamp = new_stamp(); value = Unknown }
let new_type_var () = TyVar (mk_type_var ())

let generalize env ty =
  (* Note : we use here a naive version in which generic variables are detected by
     simply checking whether they do not occur free in the englobing typing environment.
     A more efficient version would use binding levels *)
  let vars_of tvars' ty = 
     (* Returns the list of type variables occuring in [t] but not in [tvars'] *)
    let tvars = ref [] in
    let rec scan_ty t =
      match type_repr t with
      | TyVar var ->
          if not (List.memq var !tvars) && not (List.memq var tvars') 
          then tvars := var :: !tvars
      | TyArrow (t1, t2) ->
          scan_ty t1;
          scan_ty t2
      | TyProduct ts ->
          List.iter scan_ty ts
      | TyConstr (_, args, _) ->
          List.iter scan_ty args in
    scan_ty ty;
    !tvars in
  let free_tvars = 
    List.fold_left
      (fun tvs (_,ts) -> 
        let tvs' = vars_of ts.ts_params ts.ts_body in
        tvs @ tvs')
      []
      env in
  let gen_tvars = vars_of free_tvars ty in
  {ts_params = List.rev gen_tvars; ts_body = ty}

let trivial_scheme ty = {ts_params = []; ts_body = ty}

(* Type instanciation *)

let copy_type tvbs ty =
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
        TyConstr (c, List.map copy args, sz) in
  copy ty

let full_type_instance ty_sch =
  match ty_sch.ts_params with
  | [] -> ty_sch.ts_body, []
  | tparams ->
      let unknown_ts = List.map (fun var -> (var, new_type_var())) tparams in
      copy_type unknown_ts ty_sch.ts_body,
      unknown_ts

let type_instance ty_sch = fst (full_type_instance ty_sch)

let type_copy t = type_instance (generalize [] t) 

let rec pp_typ fmt t =
  let open Format in
  let pp_sz fmt sz = match sz with
    | None -> pp_print_string fmt ""
    | Some s -> fprintf fmt "[%d]" s in
  match real_type t with
  | TyConstr (c,[],sz) -> fprintf fmt "%s%a" c pp_sz sz
  | TyConstr (c,[t'],sz) -> fprintf fmt "%a %s%a" pp_typ t' c pp_sz sz
  | TyConstr (c,ts,sz) -> fprintf fmt " %a %s%a" (Rfsm.Misc.pp_list_h pp_typ) ts c pp_sz sz
  | TyProduct [t] -> pp_typ fmt t
  | TyProduct ts -> Rfsm.Misc.pp_list_h ~sep:"*" pp_typ fmt ts
  | TyArrow (t1,t2) -> fprintf fmt "%a -> %a" pp_typ t1 pp_typ t2 
  | TyVar v -> fprintf fmt "_%d" v.stamp

let pp_tvar fmt tv = Format.fprintf fmt "_%d" tv.stamp (* TO FIX *) 

let pp_typ_scheme fmt t =
  let open Format in
  match t.ts_params with
  | [] ->
     fprintf fmt "@[<h>%a@]" pp_typ t.ts_body
  | _ ->
     fprintf fmt "@[<h>forall %a. %a@]" (Rfsm.Misc.pp_list_h ~sep:"," pp_tvar) t.ts_params pp_typ t.ts_body

(* let vcd_type t =
 *   match t with
 *   | Types.TyConstr ("int", []) -> Rfsm.Vcd_types.TyInt
 *   | Types.TyConstr ("bool", []) -> Rfsm.Vcd_types.TyBool
 *   | _ -> raise Rfsm.Vcd_types.Unsupported *)
