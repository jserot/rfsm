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
             
let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.{ desc=x; typ=Types.no_type; loc=mk_location l }

(** Type declarations *)
                
type type_decl_desc = unit (* No type declaration in the Core language *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

let mk_alias_type_decl _ = Rfsm.Misc.not_implemented "type declarations for the Core language"

let rec pp_type_decl_desc fmt td = () (* No type declaration in the Core language *)
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(** Type expressions *)
                
type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc = 
  | TeConstr of string (* name, no args here *)

let rec pp_type_expr_desc fmt te = 
  let open Format in
  match te with
  | TeConstr c -> fprintf fmt "%s" c
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of Rfsm.Ident.t
  | EInt of int
  | EBool of bool
  | EBinop of Rfsm.Ident.t * expr * expr
  | ECon0 of Rfsm.Ident.t (* Nullary value constructor *)

let rec pp_expr_desc fmt e = 
  let open Format in
  match e with
  | EVar v -> fprintf fmt "%a" Rfsm.Ident.pp v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EBinop (op,e1,e2) -> fprintf fmt "%a%a%a" pp_expr e1 Rfsm.Ident.pp op pp_expr e2
  | ECon0 c -> fprintf fmt "%a" Rfsm.Ident.pp c
and pp_expr fmt e =
  pp_expr_desc fmt e.Annot.desc

(** Assignations LHS *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of Rfsm.Ident.t

let rec pp_lhs_desc ~pp_ident fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%a" pp_ident v
and pp_lhs fmt l =
  pp_lhs_desc ~pp_ident:Rfsm.Ident.pp fmt l.Annot.desc
and pp_qual_lhs fmt l =
  pp_lhs_desc ~pp_ident:Rfsm.Ident.pp_qual fmt l.Annot.desc

let is_simple_lhs l = true (* Always, for the Core language *)

let mk_simple_lhs v = Annot.{ desc=LhsVar v; typ=Types.no_type; loc=Location.no_location }

let lhs_prefix pfx l =  (* TODO: replace this by explicit scoping of Ident.t's ? *)
  let mk d = { l with Annot.desc = d } in
  let p s = pfx ^ "." ^ s in
  match l.Annot.desc with
  | LhsVar v -> mk (LhsVar Rfsm.Ident.{ v with id = p v.id })

let lhs_base_name l = match l.Annot.desc with
  | LhsVar v -> v

let lhs_vcd_repr l = match l.Annot.desc with
  | LhsVar v -> v

(** Inspectors *)
              
let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | _ -> []

let vars_of_lhs l = match l.Annot.desc with
  | LhsVar v -> [v]

(** Substitutions *)
              
let subst_var phi v = 
  try Rfsm.Subst.apply phi v
  with Not_found -> v
                    
let rec subst_id phi e =
  match e.Annot.desc with
  | EVar v -> { e with Annot.desc = EVar (subst_var phi v) }
  | EBinop (op,e1,e2) -> { e with Annot.desc = EBinop (op, subst_id phi e1, subst_id phi e2) }
  | _ -> e

let subst_lhs phi l = 
  match l.Annot.desc with
  | LhsVar v -> { l with Annot.desc = LhsVar (subst_var phi v) }

let subst_expr phi e = e  (* No parameters, hence no need to substitute *)

let subst_type_expr phi te = te  (* No parameters, hence no need to substitute *)

(** VCD interface *)
              
let vcd_name lhs =
  match lhs.Annot.desc with
  | LhsVar v -> v

(** Pre-processing *)

let is_con0_type c (t: type_expr) =
  match t.Annot.desc with
  | TeConstr c' when c=c' -> true
  | _ -> false

let is_bool_type (t: type_expr) = is_con0_type "bool" t
let is_int_type (t: type_expr) = is_con0_type "int" t
let is_event_type (t: type_expr) = is_con0_type "event" t
let is_array_type (t: type_expr) = false

let mk_bool_expr te e = match e.Annot.desc with
  | EInt 0 when is_bool_type te -> { e with Annot.desc = EBool false }
  | EInt 1 when is_bool_type te -> { e with Annot.desc = EBool true }
  | _ -> e 

let ppr_expr (env: (Rfsm.Ident.t * type_expr) list) e =
  (* Replace all bool expr [e op 0/1], where [e:bool] and [op] is [=] or [!=] by [e op false/true] *)
  let type_of v =
    (* Since pre-processing is carried out _before_ typing, the only type-related available information
       is given by the type expressions assigned to identifiers in the enclosing model *)
    try List.assoc v env
    with Not_found -> Rfsm.Misc.fatal_error "Syntax.ppr_expr" in
  let has_bool_type v = is_bool_type (type_of v) in
  match e.Annot.desc with
  | EBinop (op, ({ Annot.desc = EVar v; _ } as e'), e'') when List.mem op.Rfsm.Ident.id ["="; "!="] && has_bool_type v  ->  
       { e with Annot.desc = EBinop (op, e', mk_bool_expr (type_of v) e'') }
  | _ -> e

let ppr_lhs _ l = l 
