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

module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
             
let mk ~loc x = Annot.mk ~loc ~typ:Types.no_type x

type ident = Rfsm.Ident.t
let pp_ident = Rfsm.Ident.pp
let mk_ident = Rfsm.Ident.mk
let mk_global_ident = Rfsm.Ident.mk ~scope:Global

(** Type expressions *)

type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc = 
  | TeConstr of ident * type_expr list * type_size (* name, args, size annotations *) 

and type_size = int list (* []: none, [s] size for ints and arrays, [lo;hi] range for ints *)

let is_con_type c (t: type_expr) =
  match t.Annot.desc with
  | TeConstr (c', _, _) when c'.Rfsm.Ident.id = c -> true
  | _ -> false

let is_bool_type (t: type_expr) = is_con_type "bool" t
let is_int_type (t: type_expr) = is_con_type "int" t
let is_bit_type (t: type_expr) = is_con_type "bit" t
let is_event_type (t: type_expr) = is_con_type "event" t
let is_array_type (t: type_expr) = is_con_type "array" t


let rec pp_type_expr_desc fmt te = 
  let open Format in
  match te with
  | TeConstr (c,[],sz) -> fprintf fmt "%a%a" pp_ident c (pp_size c) sz
  | TeConstr (c,[t'],sz) -> fprintf fmt "%a %a%a" pp_type_expr t' pp_ident c (pp_size c) sz
  | TeConstr (c,ts,sz) -> fprintf fmt "(%a) %a%a" (Rfsm.Ext.List.pp_h ~sep:"," pp_type_expr) ts pp_ident c (pp_size c) sz
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

and pp_size c fmt sz =
  match sz with
  | [] -> ()
  | _ -> Format.fprintf fmt "<%a>" (Rfsm.Ext.List.pp_h ~sep:"," Format.pp_print_int) sz

(** Type declarations *)
                
type type_decl_desc =
  | TD_Enum of ident * string list (** Name, constructors *)
  | TD_Record of ident * rfield_desc list (** Name, fields *)
  | TD_Alias of ident * type_expr  (** Name, aliased type *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

and rfield_desc = string * type_expr 

let mk_alias_type_decl name te = 
  Rfsm.Annot.{ desc=TD_Alias (name, te); typ=Types.no_type; loc=Location.no_location }

let rec pp_type_decl_desc fmt td = 
  let open Format in
  let pp_rfield fmt (n,t) = fprintf fmt "%s: %a" n pp_type_expr t in
  match td with
  | TD_Enum (name,ctors) -> fprintf fmt "(\"%a\", enum { %a })" pp_ident name (Rfsm.Ext.List.pp_h ~sep:"," pp_print_string) ctors
  | TD_Record (name,fields) -> fprintf fmt "(\"%a\", record { %a })" pp_ident name (Rfsm.Ext.List.pp_h ~sep:";" pp_rfield) fields
  | TD_Alias (name,t) -> fprintf fmt "(\"%a\", alias %a)" pp_ident name pp_type_expr t
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of ident
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EChar of char
  | EBinop of ident * expr * expr
  | ECon0 of ident              (** Enum value *)
  | EIndexed of ident * expr        (** t[i] when t is an array or an int *)
  | ERanged of ident * expr * expr  (** t[hi:lo] when t is an int *)
  | EArrExt of expr list        
  | ECond of expr * expr * expr  (** e1 ? e2 : e3 *)
  | ECast of expr * type_expr
  | EFapp of ident * expr list  (** f(arg1,...,argn) *)
  | ERecord of ident * string   (** v.name when v is a record *)
  | ERecordExt of (string * expr) list  

let rec pp_expr_desc fmt e = 
  let open Format in
  let pp_rfield fmt (n,v) = fprintf fmt "%s=%a" n pp_expr v in
  match e with
  | EVar v -> fprintf fmt "%a" pp_ident v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EChar c -> fprintf fmt "%c" c
  | EBinop (op,e1,e2) -> fprintf fmt "%a%a%a" pp_expr e1 pp_ident op pp_expr e2
  | ECon0 c -> fprintf fmt "%a" pp_ident c
  | EIndexed (a,i) -> fprintf fmt "%a[%a]" pp_ident a pp_expr i
  | ERanged (a,hi,lo) -> fprintf fmt "%a[%a:%a]" pp_ident a pp_expr lo pp_expr hi
  | EArrExt vs -> fprintf fmt "[%a]" (Rfsm.Ext.List.pp_h ~sep:"," pp_expr) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" pp_expr e1 pp_expr e2 pp_expr e3
  | ECast (e,t) -> fprintf fmt "%a::%a" pp_expr e pp_type_expr t
  | EFapp (f,es) -> fprintf fmt "%a(%a)" pp_ident f (Rfsm.Ext.List.pp_h ~sep:"," pp_expr) es
  | ERecord (r,f) -> fprintf fmt "%a.%s" pp_ident r f
  | ERecordExt fs -> fprintf fmt "{%a}" (Rfsm.Ext.List.pp_h ~sep:"," pp_rfield) fs
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc

(** L-values *)
  
type lval = (lval_desc,Types.typ) Annot.t
and lval_desc = 
  | LvalVar of ident
  | LvalIndex of ident * expr (* a[i] *)
  | LvalRange of ident * expr * expr  (* v[hi:lo] := ... when v is an int *)
  | LvalRField of ident * string             (* v.field_name when v has a record type *)

let rec pp_lval_desc ~pp_ident fmt l = match l with
  | LvalVar v -> Format.fprintf fmt "%a" pp_ident v
  | LvalIndex (a,i) -> Format.fprintf fmt "%a[%a]" pp_ident a pp_expr i
  | LvalRange (a,hi,lo) -> Format.fprintf fmt "%a[%a:%a]" pp_ident a pp_expr hi pp_expr lo
  | LvalRField (r,f) -> Format.fprintf fmt "%a.%s" pp_ident r f
and pp_lval fmt l =
  pp_lval_desc ~pp_ident:Rfsm.Ident.pp fmt l.Annot.desc
and pp_qual_lval fmt l =
  pp_lval_desc ~pp_ident:Rfsm.Ident.pp_qual fmt l.Annot.desc

let mk_simple_lval v = Annot.{ desc=LvalVar v; typ=Types.no_type; loc=Location.no_location }

let is_simple_lval l = 
  match l.Annot.desc with
  | LvalVar _ ->  true
  | _ -> false

let lval_prefix pfx l =  (* TODO: replace this by explicit scoping of Ident.t's ? *)
  let mk d = { l with Annot.desc = d } in
  let p v = Rfsm.Ident.upd_id (fun x -> pfx ^ "." ^ x) v in
  match l.Annot.desc with
  | LvalVar v -> mk (LvalVar (p v))
  | LvalIndex (a,i) -> mk (LvalIndex (p a,i))
  | LvalRange (a,i1,i2) -> mk (LvalRange (p a,i1,i2))
  | LvalRField (a,f) -> mk (LvalRField (p a,f))

let lval_base_name l = match l.Annot.desc with
  | LvalVar v -> v
  | LvalIndex (a,_) -> a 
  | LvalRange (a,_,_) -> a 
  | LvalRField (a,_) -> a 

let lval_vcd_repr l = match l.Annot.desc with
  | LvalVar v -> v
  | LvalIndex (a,i) -> Rfsm.Ident.upd_id (fun x -> x ^ "." ^ Rfsm.Ext.Format.to_string pp_expr i) a 
  | LvalRange (a,hi,lo) -> a (* TO FIX ? *)
  | LvalRField (a,f) -> Rfsm.Ident.upd_id (fun x -> x ^ "." ^ f) a

(** Inspectors *)
              
let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EInt _ | EBool _ | EFloat _ | EChar _ -> []
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | ECon0 _ -> []
  | EIndexed (a,i) -> a :: vars_of_expr i
  | ERanged (v,i1,i2) -> [v] @ vars_of_expr i1 @ vars_of_expr i2
  | EArrExt vs -> List.concat (List.map vars_of_expr vs)
  | ECond (e1,e2,e3) -> vars_of_expr e1 @ vars_of_expr e2 @ vars_of_expr e3
  | ECast (e,_) -> vars_of_expr e
  | EFapp (f,es) -> List.concat (List.map vars_of_expr es)
  | ERecord (r,f) -> [r]
  | ERecordExt fs -> List.concat (List.map (fun (_,e) -> vars_of_expr e) fs)

let vars_of_lval l = match l.Annot.desc with
  | LvalVar v -> [v]
  | LvalIndex (a,i) -> a :: vars_of_expr i 
  | LvalRange (a,hi,lo) -> [a] @ vars_of_expr hi @ vars_of_expr lo
  | LvalRField (r,f) -> [r]

(** Substitution *)
              
let subst_var phi v = 
  try Rfsm.Subst.apply phi v
  with Not_found -> v
                    
let rec subst_expr phi e =
  let subst e d = { e with Annot.desc = d } in
  match e.Annot.desc with
  | EVar v -> subst e (EVar (subst_var phi v))
  | EInt _ | EBool _ | EFloat _ | EChar _ -> e
  | EBinop (op,e1,e2) -> subst e (EBinop (op, subst_expr phi e1, subst_expr phi e2))
  | ECon0 _ -> e
  | EIndexed (a,i) -> subst e (EIndexed (subst_var phi a, subst_expr phi i))
  | ERanged (a,e1,e2) -> subst e (ERanged (subst_var phi a, subst_expr phi e1, subst_expr phi e2))
  | EArrExt vs -> subst e (EArrExt (List.map (subst_expr phi) vs))
  | ECond (e1,e2,e3) -> subst e (ECond (subst_expr phi e1, subst_expr phi e2, subst_expr phi e3))
  | ECast (e,t) -> subst e (ECast (subst_expr phi e, t))
  | EFapp (f,es) -> subst e (EFapp (f, List.map (subst_expr phi) es))
  | ERecord (r,f) -> subst e (ERecord (subst_var phi r, f))
  | ERecordExt fs -> subst e (ERecordExt (List.map (fun (n,e) -> n, subst_expr phi e) fs))

let subst_lval phi l = 
  match l.Annot.desc with
  | LvalVar v -> { l with Annot.desc = LvalVar (subst_var phi v) }
  | LvalIndex (a,i) -> { l with Annot.desc = LvalIndex (subst_var phi a, subst_expr phi i) } 
  | LvalRField (r,f) -> { l with Annot.desc = LvalRField (subst_var phi r, f) } 
  | LvalRange (a,hi,lo) -> { l with Annot.desc = LvalRange (subst_var phi a, subst_expr phi hi, subst_expr phi lo) } 

let rec subst_param_expr phi e =
  let subst e d = { e with Annot.desc = d } in
  match e.Annot.desc with
  | EVar v -> if List.mem_assoc v phi then List.assoc v phi else e
  | EInt _ | EBool _ | EFloat _ | EChar _ | ECon0 _ -> e
  | EBinop (op,e1,e2) -> subst e (EBinop (op, subst_param_expr phi e1, subst_param_expr phi e2))
  | EIndexed (a,i) -> 
     (* Note. In the current system, we can apply a substitution like ["a"->{1,2,1}] to an expression like [a[i]],
        only when [i] is a litteral constant. For ex
          subst_param_expr ["a"->{1,2,1}] "a[1]" = "2"
        But this does not work when [i] is a variable or a more complex expression. 
        There are two solns
        - one would be to extend to syntax of indexed expression to accept litterals as LVAL. I.e.
          type expr = 
            ...
            | EIndexed (ilval, expr)
          and ilval = 
            | EVar ...
            | EArrLit ...
        - another would be to simply fordid arrays as parameters (!) *)
     if List.mem_assoc a phi then 
      begin match i.Annot.desc, List.assoc a phi with
        | EInt j, { Annot.desc = EArrExt vs; _ } when j >=0 && j < List.length vs -> Array.get (Array.of_list vs) j
        | _, _ -> Rfsm.Misc.not_implemented "Parameter substitution with array type"
      end
     else
       e
  | ERanged (a,e1,e2) -> subst e (ERanged (a, subst_param_expr phi e1, subst_param_expr phi e2))
  | EArrExt vs -> subst e (EArrExt (List.map (subst_param_expr phi) vs))
  | ECond (e1,e2,e3) -> subst e (ECond (subst_param_expr phi e1, subst_param_expr phi e2, subst_param_expr phi e3))
  | ECast (e,t) -> subst e (ECast (subst_param_expr phi e, subst_param_type_expr phi t))
  | EFapp (f,es) -> subst e (EFapp (f, List.map (subst_param_expr phi) es))
  | ERecord (r,f) -> e
  | ERecordExt fs -> subst e (ERecordExt (List.map (fun (n,e) -> n, subst_param_expr phi e) fs))

and subst_param_type_expr phi te = 
  match te.Annot.desc with
  | TeConstr (c,args,sz) ->
     { te with Annot.desc = TeConstr (c, List.map (subst_param_type_expr phi) args, sz) }

(** Pre-processing *)

type ppr_env = type_expr Rfsm.Env.t

let mkuminus name e =
  match name, e.Annot.desc with
  | "-", EInt n -> { e with Annot.desc = EInt (-n) }
  | ("-."|"-"), EFloat n -> { e with Annot.desc = EFloat (-.n) }
  | _ -> { e with Annot.desc = EFapp (mk_global_ident ("~"^name), [e]) }

let mk_bool_expr e = match e.Annot.desc with
  | EInt 0 -> { e with Annot.desc = EBool false }
  | EInt 1 -> { e with Annot.desc = EBool true }
  | _ -> e 

let ppr_expr env ?(expected_type=None) e =
  let type_of v =
    try Rfsm.Env.find v env
    with Not_found -> Rfsm.Misc.fatal_error "Syntax.ppr_expr" in
  let has_bool_type v = is_bool_type (type_of v) in
  match e.Annot.desc, expected_type with
  | EBinop (op, ({ Annot.desc = EVar v; _ } as e'), e''), _ when List.mem op.Rfsm.Ident.id ["="; "!="] && has_bool_type v  ->  
     (* Replace all bool expr [v {=|!=} {0|1}], where [v:bool] by [v {=|!=} {false | true}] *)
     (* TODO: extend this to [e op {0|1}] *)
     { e with Annot.desc = EBinop (op, e', mk_bool_expr e'') }
  | EInt 0, Some t when is_bool_type t -> { e with Annot.desc = EBool false }
  | EInt 1, Some t when is_bool_type t -> { e with Annot.desc = EBool true }
     (* Replace expr [0] (resp. [1]) by [false] (resp. [true]) when the expected type is [bool].
        Tests like as [e=true] or assignations like [x:=false] can be then written [e=1] and [x:=0] resp. *)
  | _ -> e

let ppr_lval _ l = l 
