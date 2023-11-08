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

exception Unsupported_type of Syntax.Types.typ
exception Unsupported_expr of Syntax.expr
                                  
open Format 

module Syntax = Syntax

let pp_type_expr fmt te = 
  let open Syntax in
  let open Rfsm.Ident in
  match te.Annot.desc with
  | TeConstr ({id="int";_},[],_) -> fprintf fmt "int" 
  | TeConstr ({id="bool";_},[],_) -> fprintf fmt "bool" 
  | TeConstr ({id="float";_},[],_) -> fprintf fmt "float" 
  | TeConstr ({id="char";_},[],_) -> fprintf fmt "char" 
  | TeConstr ({id="event";_},[],_) -> fprintf fmt "event" 
  | _ -> raise (Unsupported_type te.Annot.typ)
                    
let rec pp_type_decl_desc fmt td = 
  let open Syntax in
  let pp_rfield fmt (n,t) = fprintf fmt "%a %s; " pp_type_expr t n in
  match td with
  | TD_Enum (name,ctors) ->
     fprintf fmt "typedef enum { %a } %a;"
       (Rfsm.Ext.List.pp_h ~sep:"," pp_print_string) ctors
       pp_ident name
  | TD_Record (name,fields) ->
     fprintf fmt "typedef struct { %a} %a;"
       (Rfsm.Ext.List.pp_h pp_rfield) fields
       pp_ident name
  | TD_Alias (name,t) ->
     fprintf fmt "typedef %a %a;" pp_type_expr t pp_ident name
and pp_type_decl fmt td = Format.fprintf fmt "%a@." pp_type_decl_desc td.Syntax.Annot.desc

let pp_simple_type fmt t =
  let open Types in 
  match t with
  | TyConstr (c,[],_) -> fprintf fmt "%s" c
  | TyRecord (nm,_) -> fprintf fmt "%s" nm
  | _ -> raise (Unsupported_type t)

let pp_ident = Rfsm.Ident.pp 

let pp_expr fmt e = 
  let open Syntax in
  let paren level p = if level > 0 then p else "" in
  let cop_of op = match op.Rfsm.Ident.id with
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | "~-" -> "-" 
  | "~-." -> "-" 
  | op ->  op in
  let rec pp level fmt e = match e.Annot.desc with
  | EVar v -> fprintf fmt "%a" pp_ident v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EChar c -> fprintf fmt "'%c'" c
  | EFapp (op,[e]) when List.mem op.Rfsm.Ident.id ["~-";"~-."]-> fprintf fmt "-%a" (pp (level+1)) e
  | EFapp (f,es) -> fprintf fmt "%a(%a)" pp_ident f (Rfsm.Ext.List.pp_h ~sep:"," (pp level)) es
  | EBinop (op,e1,e2) ->
       fprintf fmt "%s%a%s%a%s" (paren level "(") (pp (level+1)) e1 (cop_of op) (pp (level+1)) e2 (paren level ")")
  | ECon0 c -> fprintf fmt "%a" pp_ident c
  | EIndexed (a,i) -> fprintf fmt "%a[%a]" pp_ident a (pp level) i
  | ERanged (a,hi,lo) -> fprintf fmt "%a[%a:%a]" pp_ident a (pp level) hi (pp level) lo (* Not strictly C *)
  | EArrExt vs -> fprintf fmt "{%a}" (Rfsm.Ext.List.pp_h ~sep:"," (pp level)) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" (pp (level+1)) e1 (pp (level+1)) e2 (pp (level+1)) e3
  | ECast (e,t) -> fprintf fmt "((%a)(%a))" pp_type_expr t (pp level) e
  | ERecordExt fs -> fprintf fmt "{%a}" (Rfsm.Ext.List.pp_h ~sep:"," (pp_rfield level)) fs (* Not strictly C *)
  | ERecord (r,f) -> fprintf fmt "%a.%s" pp_ident r f 
  and pp_rfield level fmt (n,v) = fprintf fmt "%s=%a" n (pp level) v in
  pp 0 fmt e

let pp_typed_symbol fmt (name,t) =
  let open Types in 
  match t.Syntax.Annot.typ with
  | TyConstr ("array", [t'], [sz]) -> fprintf fmt "%a %a[%d]" pp_simple_type t' pp_ident name sz
  | t -> fprintf fmt "%a %a" pp_simple_type t pp_ident name 

(* let pp_cst_decl fmt name t = 
 *   let open Types in 
 *   match t.Syntax.Annot.typ with
 *   | TyConstr ("array",_,_) -> fprintf fmt "extern %a" pp_typed_symbol (name,t)
 *   | _ -> fprintf fmt "%a" pp_typed_symbol (name,t)
 * 
 * let pp_cst_impl fmt name t v = 
 *   fprintf fmt "%a = %a" pp_typed_symbol (name,t) pp_expr v *)

