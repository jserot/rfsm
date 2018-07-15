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
  | TyUnknown
  | TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option
  | TyFloat
  | TyArray of int * typ    (* size, subtype *)
  | TyVar of tvar           (* Only used internally for type checking *)
  | TyArrow of typ * typ    (* Only used internally for type checking *)
  | TyProduct of typ list   (* Only used internally for type checking *)

and tvar =
  { stamp: string;             (* for debug only *)
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

and int_range = Index.t * Index.t (* min, max *)

type typ_scheme =
  { ts_params: tvar list;
    ts_body: typ }

(* Type variables *)
  
let new_stamp =
  let var_cnt = ref 0 in
  function () -> incr var_cnt; "_" ^ string_of_int !var_cnt
let mk_type_var () = { value = Unknown; stamp=new_stamp () }
let new_type_var () = TyVar (mk_type_var ())

let rec type_repr = function
  | TyVar ({value = Known ty1} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

and real_type ty = 
  match type_repr ty with
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyArray (sz, ty') -> TyArray (sz, real_type ty')
  | TyVar { value=Known ty'} -> ty'
  | ty -> ty


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
    | TyArray (sz, ty') ->
         TyArray (sz, copy ty')
    | ty -> ty in
  copy ty

let type_instance ty_sch =
  match ty_sch.ts_params with
  | [] -> ty_sch.ts_body
  | params ->
      let unknowns = List.map (fun var -> (var, new_type_var())) params in
      copy_type unknowns ty_sch.ts_body

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
  | TyInt (Some (lo1,hi1)), TyInt (Some (lo2,hi2)) ->
     if lo1 <> lo2 || hi1 <> hi2 then raise (TypeConflict(val1, val2))
  | TyInt _, TyInt _ -> ()
  | TyBool, TyBool -> ()
  | TyEvent, TyEvent  -> ()
  | TyEnum cs1, TyEnum cs2 when cs1=cs2 -> ()
  | _, _ ->
      raise (TypeConflict(val1, val2))

and occur_check var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' -> if var == var' then raise(TypeCircularity(TyVar var, ty))
    | TyArrow (ty1,ty2) -> test ty1; test ty2
    | TyProduct ts -> List.iter test ts
    | _ -> ()
  in test ty


let ivars_of = function
  | TyInt (Some (lo,hi)) -> VarSet.elements (VarSet.union (Index.vars_of lo) (Index.vars_of hi))
  | _ -> []

(* Index manipulation *)
       
let subst_indexes env ty =
    match ty with
    | TyInt (Some (hi, lo)) -> TyInt (Some (Index.subst env hi, Index.subst env lo))
    | _ -> ty

(* Checking *)

let is_event_type t = match t with 
  | TyEvent -> true
  | _ -> false

let rec type_equal ~strict t1 t2 =
  match real_type t1, real_type t2 with
  | TyBool, TyBool -> true
  | TyEvent, TyEvent -> true
  | TyInt (Some (lo1,hi1)), TyInt (Some (lo2,hi2)) -> if strict then lo1=lo2 && hi1=hi2 else true
  | TyInt (Some _), TyInt None
  | TyInt None, TyInt (Some _) -> if strict then false else true
  | TyInt None, TyInt None -> true
  | TyFloat, TyFloat -> true
  | TyEnum cs1, TyEnum cs2 ->
     if strict then List.sort compare cs1 = List.sort compare cs2
     else List.for_all (function c -> List.mem c cs1) cs2
        (* so that, for ex, [type_equal ~strict:false {On,Off} {On} = true] *)
  | TyVar { stamp=s1; value=Unknown }, TyVar { stamp=s2; value=Unknown } -> s1 = s2
  | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
      type_equal ~strict ty1 ty2 && type_equal ~strict ty1' ty2'
  | TyProduct ts, TyProduct ts' when List.length ts = List.length ts'->
      List.for_all2 (type_equal ~strict) ts ts'
  | TyArray (sz1, ty1), TyArray (sz2, ty2) -> 
     sz1 = sz2 && type_equal ~strict ty1 ty2
  | _, _ -> false

(* Accessors *)
                 
let rec enums_of ty = match ty with
  | TyEnum cs -> List.map (function c -> c, ty) cs
  | _ -> []

let size_of ty = match ty with
  | TyArray (sz, _) -> sz
  | TyProduct ts -> List.length ts
  | _ -> 0

let subtype_of = function
  | TyArray (_,t) -> t
  | _ -> failwith "Types.array_subtype"
       
(* Printing *)

let string_of_range (lo,hi) = Index.to_string lo ^ ":" ^ Index.to_string hi

let rec string_of_type t = match t with 
  | TyUnknown -> "<unknown>"
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum cs -> "{" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt None -> "int"
  | TyInt (Some (lo,hi)) -> "int<" ^ string_of_range (lo,hi) ^ ">"
  | TyFloat -> "float"
  | TyVar v -> v.stamp
  | TyArrow (t1,t2) -> string_of_type t1 ^ "->" ^ string_of_type t2
  | TyProduct ts -> Utils.ListExt.to_string string_of_type "*" ts 
  | TyArray (sz,ty') -> string_of_type ty' ^ " array[" ^ string_of_int sz ^ "]"

let string_of_type_scheme ts = "[]" ^ string_of_type ts.ts_body (* TOFIX *)
