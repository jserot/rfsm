(* The implementation defined in this file should match the signature [Guest.SYNTAX]  specified in ../../../host/lib/guest.ml *)

module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
module Ident = Rfsm.Ident

let mk ~loc x = Annot.mk ~loc ~typ:Types.no_type x

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

(* L-values *)
  
type lval = (lval_desc,Types.typ) Annot.t
and lval_desc = Ident.t

let lval_var l = Ident.to_string l.Annot.desc

let vars_of_lval l = [l.Annot.desc]

let is_simple_lval l = true

let mk_simple_lval v =
  Annot.{ desc=v; typ=Types.no_type; loc=Location.no_location }

let lval_prefix pfx l = { l with Annot.desc = Ident.upd_id (fun i -> pfx^"."^i) l.Annot.desc }

let lval_base_name l = l.Annot.desc

let lval_vcd_repr l = l.Annot.desc

let pp_lval fmt l = Format.fprintf fmt "%a" Ident.pp l.Annot.desc
let pp_qual_lval fmt l = Format.fprintf fmt "%a" Ident.pp_qual l.Annot.desc

(* Substitutions *)
              
let subst phi v = 
  try List.assoc v phi
  with Not_found -> v

let subst_expr phi e =
  match e.Annot.desc with
  | EVar v -> { e with Annot.desc = EVar (subst phi v) }
  | _ -> e

let subst_lval phi l = { l with Annot.desc = subst phi l.Annot.desc }

let subst_param_expr phi e = e (* No parameters in this guest language *)

let subst_param_type_expr phi te =  te (* No parameters in this guest language *)

(* Pre-processing *)

type ppr_env = type_expr Rfsm.Env.t

let ppr_expr env ?(expected_type=None) e = e 

let ppr_lval env l = l
