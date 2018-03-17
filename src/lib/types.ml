type date = int

type typ = 
  | TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option

and int_range = type_index * type_index (* min, max *)

and type_index =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index * type_index

exception Illegal_type_index of string * Expr.value 
exception Unbound_type_index of string

type env = (string * Expr.value) list

let rec subst_index env i = match i with 
    TiConst _ -> i
  | TiVar v ->
     begin
       match List.assoc v env with
       | Expr.Val_int c -> TiConst c
       | e -> raise (Illegal_type_index (v,e))
       | exception Not_found -> raise (Unbound_type_index v)
     end
  | TiBinop (op,e1,e2) ->
     begin match subst_index env e1, subst_index env e2 with
     | TiConst c1, TiConst c2 ->
        let f = Expr.Builtins.lookup Expr.Builtins.binops op in
        TiConst (f c1 c2)
     | i1, i2 ->
        TiBinop (op, i1, i2)
     end

let subst env ty = match ty with
  | TyInt (Some (hi, lo)) -> TyInt (Some (subst_index env hi, subst_index env lo))
  | _ -> ty

module VarSet = Set.Make(struct type t = string let compare = Pervasives.compare end)
                 
let rec vars_of_index = function
  | TiConst _ -> VarSet.empty
  | TiVar v -> VarSet.singleton v
  | TiBinop (_,e1,e2) -> VarSet.union (vars_of_index e1) (vars_of_index e2)

let vars_of = function
  | TyInt (Some (lo,hi)) -> VarSet.elements (VarSet.union (vars_of_index lo) (vars_of_index hi))
  | _ -> []

(* Type checking *)

let is_event_type t = match t with 
  | TyEvent -> true
  | _ -> false

let type_equal env ty ty' =  
  subst env ty = subst env ty'   (* Structural equality modulo index instanciation *)
                                      
(* Printing *)

let rec string_of_type_index = function
    TiConst c -> string_of_int c
  | TiVar v -> v
  | TiBinop (op,e1,e2) -> string_of_type_index e1 ^ op ^ string_of_type_index e2
                
let string_of_range (lo,hi) = string_of_type_index lo ^ ".." ^ string_of_type_index hi

let string_of_type t = match t with 
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum cs -> "{" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt None -> "int"
  | TyInt (Some (lo,hi)) -> "int<" ^ string_of_range (lo,hi) ^ ">"
