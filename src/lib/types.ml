type date = int

type dir = IO_In | IO_Out | IO_Inout
                          
type typ = 
  | TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option
  | TyVar of tvar           (* Only used internally for type checking *)
  | TyArrow of typ * typ    (* Only used internally for type checking *)
  | TyProduct of typ list   (* Only used internally for type checking *)

and tvar =
  { stamp: string;             (* for debug only *)
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

and int_range = type_index * type_index (* min, max *)

and type_index =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index * type_index

type typ_scheme =
  { ts_params: tvar list;
    ts_body: typ }

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
  | TyInt (Some (lo1,hi1)), TyInt (Some (lo2,hi2)) ->
     if lo1 <> lo2 || hi1 <> hi2 then raise (TypeConflict(val1, val2))
  | TyInt _, TyInt _ -> ()
  | TyBool, TyBool -> ()
  | TyEvent, TyEvent  -> ()
  | TyEnum cs1, TyEnum cs2 when cs1=cs2 -> ()
  | _, _ ->
      raise (TypeConflict(val1, val2))

and real_type ty = 
  match type_repr ty with
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyVar { value=Known ty'} -> ty'
  | ty -> ty

and type_repr = function
  | TyVar ({value = Known ty1} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

and occur_check var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' -> if var == var' then raise(TypeCircularity(TyVar var, ty))
    | TyArrow (ty1,ty2) -> test ty1; test ty2
    | TyProduct ts -> List.iter test ts
    | _ -> ()
  in test ty

(* Type index manipulation *)

exception Illegal_type_index of string * Expr.value 
exception Unbound_type_index of string

type ienv = (string * Expr.value) list

let subst_indexes ienv ty =
  let rec subst i = match i with 
    TiConst _ -> i
  | TiVar v ->
     begin
       match List.assoc v ienv with
       | Expr.Val_int c -> TiConst c
       | e -> raise (Illegal_type_index (v,e))
       | exception Not_found -> raise (Unbound_type_index v)
     end
  | TiBinop (op,e1,e2) ->
     begin match subst e1, subst e2 with
     | TiConst c1, TiConst c2 ->
        let f = Expr.Builtins.lookup Expr.Builtins.binops op in
        TiConst (f c1 c2)
     | i1, i2 ->
        TiBinop (op, i1, i2)
     end in
  match ty with
  | TyInt (Some (hi, lo)) -> TyInt (Some (subst hi, subst lo))
  | _ -> ty

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
let rec vars_of_index = function
  | TiConst _ -> VarSet.empty
  | TiVar v -> VarSet.singleton v
  | TiBinop (_,e1,e2) -> VarSet.union (vars_of_index e1) (vars_of_index e2)

let vars_of = function
  | TyInt (Some (lo,hi)) -> VarSet.elements (VarSet.union (vars_of_index lo) (vars_of_index hi))
  | _ -> []

(* Typing *)

let new_stamp =
  let var_cnt = ref 0 in
  function () -> incr var_cnt; "_" ^ string_of_int !var_cnt
let mk_type_var () = { value = Unknown; stamp=new_stamp () }
let new_type_var () = TyVar (mk_type_var ())
                             
exception Unbound_id of string * string 
exception Typing_mismatch of Expr.t * string 
exception Typing_error of Expr.t * typ * typ 
                        
type tenv =
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

let lookup_type what env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id (what, id))

let lookup_type_scheme env id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id ("builtin operator", id))
                  
let rec type_expression tenv expr = match expr with
    Expr.EInt c -> TyInt None
  | Expr.EBool b -> TyBool
  | Expr.EVar id -> lookup_type "variable" tenv.te_vars id
  | Expr.EEnum c ->  lookup_type "enum value" tenv.te_ctors c
  | Expr.EBinop (op,e1,e2) ->
      let ty_fn = type_instance (lookup_type_scheme tenv.te_prims op) in
      let ty_arg = TyProduct (List.map (type_expression tenv) [e1;e2]) in
      let ty_result = new_type_var () in
      begin
        try 
          (* Printf.printf "** unifying %s and %s -> %s\n" (string_of_type ty_fn) (string_of_type ty_arg) (string_of_type ty_result); *)
          unify ty_fn (TyArrow (ty_arg,ty_result));
          (* Printf.printf "** done: -> %s\n" (string_of_type (real_type ty_result)); flush stdout; *)
          real_type ty_result
        with
           TypeConflict (t,t')
         | TypeCircularity(t,t') -> raise (Typing_error (expr, t, t'))
      end

and type_instance ty_sch =
  match ty_sch.ts_params with
  | [] -> ty_sch.ts_body
  | params ->
      let unknowns = List.map (fun var -> (var, new_type_var())) params in
      copy_type unknowns ty_sch.ts_body

and copy_type tvbs ty =
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
    | ty -> ty in
  copy ty

(* Checking *)

let is_event_type t = match t with 
  | TyEvent -> true
  | _ -> false

let rec type_equal ~strict t1 t2 =
  match real_type t1, real_type t2 with
  | TyBool, TyBool -> true
  | TyEvent, TyEvent -> true
  | TyEnum cs1, TyEnum cs2 -> List.sort compare cs1 = List.sort compare cs2
  | TyInt (Some (lo1,hi1)), TyInt (Some (lo2,hi2)) -> if strict then lo1=lo2 && hi1=hi2 else true
  | TyInt (Some _), TyInt None
  | TyInt None, TyInt (Some _) -> if strict then false else true
  | TyInt None, TyInt None -> true
  | TyVar { stamp=s1; value=Unknown }, TyVar { stamp=s2; value=Unknown } -> s1 = s2
  | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
      type_equal ~strict ty1 ty2 && type_equal ~strict ty1' ty2'
  | TyProduct ts, TyProduct ts' when List.length ts = List.length ts'->
      List.for_all2 (type_equal ~strict) ts ts'
  | _, _ -> false

let type_of_value = function
  | Expr.Val_int _ -> TyInt None
  | Expr.Val_bool _ -> TyBool
  | Expr.Val_enum c -> TyEnum [c]  (* TO FIX *)

(* Builtin typing environment *)

let type_arithm2 () = 
  { ts_params=[]; ts_body=TyArrow (TyProduct [TyInt None; TyInt None], TyInt None) }

let type_compar () = 
  let tv = mk_type_var () in
  { ts_params = [tv]; ts_body=TyArrow (TyProduct [TyVar tv; TyVar tv], TyBool) }

let builtin_tenv = {
  te_vars = [];
  te_ctors = [
   "True", TyBool;
   "False", TyBool
   ];
  te_prims = [
    "+", type_arithm2 ();
    "-", type_arithm2 ();
    "*", type_arithm2 ();
    "/", type_arithm2 ();
    "mod", type_arithm2 ();
    "=", type_compar ();
    "!=", type_compar ();
    "<", type_compar ();
    ">", type_compar ();
    ">=", type_compar ();
    "<=", type_compar ()
    ]
  }

(* Accessors *)
                 
let rec tycons_of ty = match ty with
  | TyEnum cs -> List.map (function c -> c, ty) cs
  | _ -> []

(* Printing *)

let rec string_of_type_index = function
    TiConst c -> string_of_int c
  | TiVar v -> v
  | TiBinop (op,e1,e2) -> string_of_type_index e1 ^ op ^ string_of_type_index e2
                
let string_of_range (lo,hi) = string_of_type_index lo ^ ".." ^ string_of_type_index hi

let rec string_of_type t = match t with 
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum cs -> "{" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt None -> "int"
  | TyInt (Some (lo,hi)) -> "int<" ^ string_of_range (lo,hi) ^ ">"
  | TyVar v -> v.stamp
  | TyArrow (t1,t2) -> string_of_type t1 ^ "->" ^ string_of_type t2
  | TyProduct ts -> Utils.ListExt.to_string string_of_type "*" ts 

let string_of_type_scheme ts = "[]" ^ string_of_type ts.ts_body (* TOFIX *)
                             

let dump_tenv tenv =  (* For debug only *)
  Printf.printf "te.vars = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_vars);
  Printf.printf "te.ctors = %s\n"
    (Utils.ListExt.to_string (function (id,ty) -> id ^ ":" ^ string_of_type ty) ", " tenv.te_ctors);
  Printf.printf "te.prims = %s\n"
    (Utils.ListExt.to_string (function (id,ts) -> id ^ ":" ^ string_of_type_scheme ts) ", " tenv.te_prims)
