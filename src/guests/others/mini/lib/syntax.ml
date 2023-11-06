(* The implementation defined in this file should match the signature [Guest.SYNTAX]  specified in ../../../host/lib/guest.ml *)

module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
module Ident = Rfsm.Ident

let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.{ desc=x; typ=Types.no_type; loc=mk_location l }

(* Type expressions *)
                
type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc =
  | TeConstr of string (* name, no args here *)

let is_bool_type (t: type_expr) = match t.Annot.desc with TeConstr "bool" -> true | _ -> false
let is_event_type (t: type_expr) = match t.Annot.desc with TeConstr "event" -> true | _ -> false
let is_array_type (t: type_expr) = false

let rec pp_type_expr_desc fmt te =
  match te with TeConstr c -> Format.fprintf fmt "%s" c
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

(* Type declarations *)
                
type type_decl_desc = unit (* No type declaration in this guest language *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

let mk_alias_type_decl _ = Rfsm.Misc.not_implemented "type declarations for the Core language"

let pp_type_decl fmt td = ()

(* Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of Ident.t
  | EBool of bool

let vars_of_expr e = [] 

let rec pp_expr_desc fmt e = 
  match e with 
  | EVar v -> Format.fprintf fmt "%a" Ident.pp v
  | EBool b -> Format.fprintf fmt "%b" b
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc

(* LHSs *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = Ident.t

let lhs_var l = Ident.to_string l.Annot.desc

let vars_of_lhs l = [l.Annot.desc]

let is_simple_lhs l = true

let mk_simple_lhs v =
  Annot.{ desc=v; typ=Types.no_type; loc=Location.no_location }

let lhs_prefix pfx l = { l with Annot.desc = Ident.upd_id (fun i -> pfx^"."^i) l.Annot.desc }

let lhs_base_name l = l.Annot.desc

let lhs_vcd_repr l = l.Annot.desc

let pp_lhs fmt l = Format.fprintf fmt "%a" Ident.pp l.Annot.desc
let pp_qual_lhs fmt l = Format.fprintf fmt "%a" Ident.pp_qual l.Annot.desc

(* Substitutions *)
              
let subst phi v = 
  try List.assoc v phi
  with Not_found -> v

let subst_id phi e =
  match e.Annot.desc with
  | EVar v -> { e with Annot.desc = EVar (subst phi v) }
  | _ -> e

let subst_lhs phi l = { l with Annot.desc = subst phi l.Annot.desc }

let subst_expr phi e = e (* No parameters in this guest language *)

let subst_type_expr phi te =  te (* No parameters in this guest language *)

(* Pre-processing *)

type ppr_env = type_expr Rfsm.Env.t

let ppr_expr env ?(expected_type=None) e = e 

let ppr_lhs env l = l
