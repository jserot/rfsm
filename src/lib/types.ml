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

type date = int

type dir = IO_In | IO_Out | IO_Inout

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
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

  let rec subst env i = match i with 
        TiConst _ -> i
      | TiVar v ->
         begin
           try TiConst (List.assoc v env)
           with Not_found -> raise (Unbound_type_index v)
         end
      | TiBinop (op,e1,e2) ->
         begin match subst env e1, subst env e2 with
         | TiConst c1, TiConst c2 -> TiConst ((lookup op) c1 c2)
         | i1, i2 -> TiBinop (op, i1, i2)
         end 

  let rec vars_of = function
    | TiConst _ -> VarSet.empty
    | TiVar v -> VarSet.singleton v
    | TiBinop (_,e1,e2) -> VarSet.union (vars_of e1) (vars_of e2)

  let rec to_string = function
    TiConst c -> string_of_int c
  | TiVar v -> v
  | TiBinop (op,e1,e2) -> to_string e1 ^ op ^ to_string e2 (* TODO: add parens *)
                
end

type typ =
  | TyEvent
  | TyBool
  | TyEnum of name * string list              (** Name, list of values *)
  | TyInt of siz                
  | TyFloat
  | TyChar
  | TyArray of Index.t * typ                        (** size, subtype *)
  | TyVar of typ var                                (** Internal use only *)
  | TyArrow of typ * typ                            (** Internal use only *)
  | TyProduct of typ list                           (** Internal use only *)
  | TyRecord of name * (string * typ) list    (** Name, fields *)

and siz =
  | SzExpr1 of Index.t                  (* For ints: bit width, for arrays: dimension *)
  | SzExpr2 of Index.t * Index.t        (* For ints: range, for arrays: dimensions *)
  | SzVar of siz var   

and name =
  | NmLit of string
  | NmVar of name var   

and 'a var =
  { stamp: string;             (* for debug only *)
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
  let var_cnt = ref 0 in
  function () -> incr var_cnt; "_" ^ string_of_int !var_cnt

let make_var () = { value = Unknown; stamp=new_stamp () }
let new_type_var () = TyVar (make_var ())
let new_size_var () = SzVar (make_var ())
let new_name_var () = NmVar (make_var ())

(* Builders *)

let no_type = TyProduct []
            
let type_int = function
    [] -> TyInt (new_size_var ())
  | [w] -> TyInt (SzExpr1 (TiConst w))
  | [lo;hi] -> TyInt (SzExpr2 (TiConst lo, TiConst hi))
  | _ -> invalid_arg "Types.type_int"

let rec type_repr = function
  | TyVar ({value = Known ty1} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

let rec size_repr = function
  | SzVar ({value = Known sz1} as var) ->
      let sz = size_repr sz1 in
      var.value <- Known sz;
      sz
  | sz -> sz

let rec name_repr = function
  | NmVar ({value = Known nm1} as var) ->
      let nm = name_repr nm1 in
      var.value <- Known nm;
      nm
  | nm -> nm

(* Real type : path compression + unabbreviation *)

let rec real_type ty = 
  match type_repr ty with
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyArray (sz, ty') -> TyArray (sz, real_type ty')
  | TyVar { value=Known ty'} -> ty'
  | TyInt sz -> TyInt (real_size sz)
  | TyEnum (name, desc) -> TyEnum (real_name name, desc)
  | TyRecord (name, fds) -> TyRecord (real_name name, List.map (function (n,ty') -> n, real_type ty') fds)
  | ty -> ty

and real_size sz = 
  match size_repr sz with
  | SzVar { value=Known sz'} -> sz'
  | sz -> sz

and real_name nm = 
  match name_repr nm with
  | NmVar { value=Known nm'} -> nm'
  | nm -> nm

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
    | TyArray (sz, ty') ->
       TyArray (sz, copy ty')
    | TyInt sz ->
       TyInt (copy_size ty svbs sz)
    | TyEnum (nm, desc) ->
       TyEnum (nm, desc)
    | TyRecord (nm, fds) ->
       TyRecord(nm, List.map (function (n,t) -> n, copy t) fds)
    | ty -> ty in
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

exception TypeConflict of typ * typ
exception TypeCircularity of typ * typ

let rec unify ty1 ty2 =
  let val1 = real_type ty1
  and val2 = real_type ty2 in
  if val1 == val2 then () else
  match (val1, val2) with
  | TyVar v1, TyVar v2 when v1==v2 -> 
      ()
  | TyVar var, ty ->
      occur_check var ty;
      var.value <- Known ty
  | ty, TyVar var ->
      occur_check var ty;
      var.value <- Known ty
  | TyArrow(ty1, ty2), TyArrow(ty1', ty2') ->
      unify ty1 ty1';
      unify ty2 ty2'
  | TyProduct ts1, TyProduct ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 unify ts1 ts2
  | TyArray (sz1, ty1), TyArray (sz2, ty2) when sz1 = sz2 ->
     unify ty1 ty2
  | TyInt sz1, TyInt sz2 ->
     unify_size (val1,val2) sz1 sz2
  | TyBool, TyBool -> ()
  | TyEvent, TyEvent  -> ()
  | TyFloat, TyFloat  -> ()
  | TyChar, TyChar  -> ()
  | TyEnum (nm1,cs1), TyEnum (nm2,cs2) ->
     if List.sort compare cs1 = List.sort compare cs2
     then unify_name (val1,val2) nm1 nm2
     else raise (TypeConflict(val1, val2))
  | TyRecord (nm1,fds1), TyRecord (nm2,fds2) ->
     List.iter2
       (fun (n1,t1) (n2,t2) ->
         if n1 = n2
         then unify t1 t2
         else raise (TypeConflict(val1, val2)))
       fds1 fds2;
     unify_name (val1,val2) nm1 nm2
  | _, _ ->
      raise (TypeConflict(val1, val2))

and unify_size (ty1,ty2) sz1 sz2 =
  let val1 = real_size sz1
  and val2 = real_size sz2 in
  if val1 == val2 then
    ()
  else
  match (val1, val2) with
    | SzVar var1, SzVar var2 when var1 == var2 ->  (* This is hack *)
        ()
    | SzVar var, sz ->
        occur_check_size (ty1,ty2) var sz;
        var.value <- Known sz
    | sz, SzVar var ->
        occur_check_size (ty1,ty2) var sz;
        var.value <- Known sz
    | SzExpr1 (TiConst w1), SzExpr1 (TiConst w2) when w1 = w2 ->
        ()
    | SzExpr2 (TiConst lo1, TiConst hi1), SzExpr2 (TiConst lo2, TiConst hi2) when lo1 = lo2 && hi1 = hi2 ->
        ()
    | _, _ ->
        raise (TypeConflict(ty1, ty2))

and unify_name (ty1,ty2) nm1 nm2 =
  let val1 = real_name nm1
  and val2 = real_name nm2 in
  if val1 == val2 then
    ()
  else
  match (val1, val2) with
    | NmLit s1, NmLit s2 when s1 = s2 -> ()
    | NmVar var1, NmVar var2 when var1 == var2 -> () (* This is hack *)
    | NmVar var, nm -> var.value <- Known nm
    | nm, NmVar var -> var.value <- Known nm
    | _, _ ->
        raise (TypeConflict(ty1, ty2))

and occur_check var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' -> if var == var' then raise(TypeCircularity(TyVar var, ty))
    | TyArrow (ty1,ty2) -> test ty1; test ty2
    | TyProduct ts -> List.iter test ts
    | _ -> ()
  in test ty

and occur_check_size (ty1,ty2) var sz =
  let rec test s =
    match size_repr s with
    | SzVar var' ->
        if var == var' then raise(TypeCircularity(ty1,ty2))
    | _ ->
        ()
  in test sz

let ivars_of = function
  | TyInt (SzExpr1 sz) -> VarSet.elements (Index.vars_of sz)
  | TyInt (SzExpr2 (lo,hi)) -> VarSet.elements (VarSet.union (Index.vars_of lo) (Index.vars_of hi))
  | TyArray (sz, ty) -> VarSet.elements (Index.vars_of sz)
  | _ -> []

(* Index manipulation *)
       
let subst_indexes env ty =
    match ty with
    | TyInt (SzExpr1 sz) -> TyInt (SzExpr1 (Index.subst env sz))
    | TyInt (SzExpr2 (hi,lo)) -> TyInt (SzExpr2 (Index.subst env hi, Index.subst env lo))
    | TyArray (sz, ty') -> TyArray (Index.subst env sz, ty')
    | _ -> ty

(* Checking *)

let is_event_type t = match t with 
  | TyEvent -> true
  | _ -> false

let rec type_equal ~strict t1 t2 =
  match real_type t1, real_type t2 with
  | TyBool, TyBool -> true
  | TyEvent, TyEvent -> true
  | TyInt sz1, TyInt sz2 -> size_equal ~strict:strict sz1 sz2
  | TyFloat, TyFloat -> true
  | TyChar, TyChar -> true
  | TyEnum (nm1,cs1), TyEnum (nm2,cs2) ->
     name_equal ~strict nm1 nm2 && 
     (if strict then List.sort compare cs1 = List.sort compare cs2
     else List.for_all (function c -> List.mem c cs1) cs2)
        (* so that, for ex, [type_equal ~strict:false {On,Off} {On} = true] *)
  | TyVar { stamp=s1; value=Unknown }, TyVar { stamp=s2; value=Unknown } -> s1 = s2
  | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
      type_equal ~strict ty1 ty2 && type_equal ~strict ty1' ty2'
  | TyProduct ts, TyProduct ts' when List.length ts = List.length ts'->
      List.for_all2 (type_equal ~strict) ts ts'
  | TyArray (sz1, ty1), TyArray (sz2, ty2) -> 
     sz1 = sz2 && type_equal ~strict ty1 ty2
  | TyRecord (nm1, fds1), TyRecord (nm2, fds2) ->
     name_equal ~strict nm1 nm2 && List.for_all2 (fun (n1,t1) (n2,t2) -> type_equal ~strict t1 t2) fds1 fds2
  | _, _ -> false

and size_equal ~strict s1 s2 =
  match real_size s1, real_size s2 with
  | SzExpr1 w1, SzExpr1 w2 -> w1 = w2
  | SzExpr2 (lo1,hi1), SzExpr2 (lo2,hi2) -> lo1 = lo2 && hi1 = hi2
  | SzVar v1, SzVar v2 -> v1 == v2
  | _, _ -> false

and name_equal ~strict nm1 nm2 =
  match real_name nm1, real_name nm2 with
  | NmLit s1, NmLit s2 -> s1 = s2
  | NmVar v1, NmVar v2 -> v1 == v2
  | _, _ -> false
    
(* Accessors *)
                 
let rec enums_of ty = match ty with
  | TyEnum (_,cs) -> List.map (function c -> c, ty) cs
  | _ -> []

let size_of ty = match ty with
  | TyArray (TiConst sz, _) -> sz
  | TyProduct ts -> List.length ts
  | _ -> 0

let subtype_of = function
  | TyArray (_,t) -> t
  | _ -> Misc.fatal_error "Types.subtype_of"
       
let is_lit_name nm = match real_name nm with
  | NmLit _ -> true
  | _ -> false
       
(* Printing *)

let string_of_range (lo,hi) = Index.to_string lo ^ ":" ^ Index.to_string hi

let rec string_of_type ?(szvars=false) t = match t with 
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum (nm, cs) ->
     begin match real_name nm with
       NmLit s -> s
     | _ -> "{" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
     end
  | TyInt sz -> "int" ^ string_of_size ~szvars sz
  | TyFloat -> "float"
  | TyChar -> "char"
  | TyVar v -> v.stamp
  | TyArrow (t1,t2) -> string_of_type t1 ^ "->" ^ string_of_type t2
  | TyProduct ts -> Utils.ListExt.to_string string_of_type "*" ts 
  | TyArray (sz,ty') -> string_of_type ty' ^ " array[" ^ Index.to_string sz ^ "]"
  | TyRecord (nm,fs) -> 
     begin match real_name nm with
       NmLit s -> s
     | _ -> "{" ^ Utils.ListExt.to_string string_of_field "," fs ^ "}"
     end

and string_of_size ?(szvars=false) sz =
  let s = match size_repr sz with
  | SzVar v -> if szvars then v.stamp else ""
  | SzExpr1 e -> Index.to_string e
  | SzExpr2 (e1,e2) -> Index.to_string e1 ^ ":" ^ Index.to_string e2 in
  match s with
  | "" -> ""
  | _ -> "<" ^ s ^ ">"

and string_of_name nm = match name_repr nm with
    | NmLit s -> s
    | NmVar v -> v.stamp 

and string_of_field (n,ty) = n ^ ":" ^ string_of_type ty

let string_of_type_scheme ts = "[]" ^ string_of_type ts.ts_body (* TOFIX *)
