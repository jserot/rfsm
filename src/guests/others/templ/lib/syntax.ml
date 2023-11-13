(* Template for defining the guest language syntax *)
(* The implementation defined in this file should match the signature [Guest.SYNTAX]  specified in ../../../host/lib/guest.ml *)

module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot

let mk ~loc x = Annot.mk ~loc ~typ:Types.no_type x

(* Type expressions *)
                
type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc =  (* TO BE FILLED *)

let is_bool_type (t: type_expr) = (* TO BE FILLED *)
let is_event_type (t: type_expr) = (* TO BE FILLED *)
let is_array_type (t: type_expr) =(* TO BE FILLED *)

let rec pp_type_expr_desc fmt te = (* TO BE FILLED *)
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

(* Type declarations *)
                
type type_decl_desc =  (* TO BE FILLED *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

let mk_alias_type_decl _ = (* TO BE FILLED *)

let rec pp_type_decl_desc fmt td = (* TO BE FILLED *)
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(* Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc =  (* TO BE FILLED *)

let rec vars_of_expr e =  (* TO BE FILLED *)

let rec pp_expr_desc fmt e =  (* TO BE FILLED *)
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc

(* LHSs *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc =  (* TO BE FILLED *)

let vars_of_lhs l =  (* TO BE FILLED *)

let is_simple_lhs l = (* TO BE FILLED *)

let mk_simple_lhs v =  (* TO BE FILLED *)

let lhs_prefix pfx l =   (* TO BE FILLED *)

let lhs_base_name l =  (* TO BE FILLED *)

let lhs_vcd_repr l =  (* TO BE FILLED *)

let rec pp_lhs_desc ~pp_ident fmt l = (* TO BE FILLED *)
and pp_lhs fmt l = pp_lhs_desc ~pp_ident:Rfsm.Ident.pp fmt l.Annot.desc
and pp_qual_lhs fmt l = pp_lhs_desc ~pp_ident:Rfsm.Ident.pp_qual fmt l.Annot.desc

(* Substitutions *)
              
let rec subst_id phi e = (* TO BE FILLED. Hint: use [Rfsm.Subst.apply] *)

let subst_lhs phi l =  (* TO BE FILLED. Hint: use [Rfsm.Subst.apply] *)

let subst_expr phi e = (* TO BE FILLED *)

let subst_type_expr phi te =  (* TO BE FILLED *)

(* Pre-processing *)

type ppr_env = type_expr Rfsm.Env.t


let ppr_expr env ?(expected_type=None) e = (* TO BE FILLED *)

let ppr_lhs env l = (* TO BE FILLED *)
