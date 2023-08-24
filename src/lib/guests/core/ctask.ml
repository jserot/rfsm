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
  match te.Annot.desc with
  | TeConstr c -> fprintf fmt "%s" c 
                    
let pp_type_decl fmt td = () (* No type declarations for Core language *)

let pp_simple_type fmt t =
  let open Types in 
  match t with
  | TyConstr (c,[]) -> fprintf fmt "%s" c
  | _ -> raise (Unsupported_type t)

let pp_expr fmt e = 
  let open Syntax in
  let paren level p = if level > 0 then p else "" in
  let cop_of op = match op.Rfsm.Ident.id with
    "=" -> "=="
  | "mod" -> "%"
  | "~-" -> "-" 
  | op ->  op in
  let rec pp level fmt e = match e.Annot.desc with
  | EVar v -> fprintf fmt "%a" Rfsm.Ident.pp v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | ECon0 c ->  fprintf fmt "%a" Rfsm.Ident.pp c            
  | EBinop (op,e1,e2) ->
       fprintf fmt "%s%a%s%a%s" (paren level "(") (pp (level+1)) e1 (cop_of op) (pp (level+1)) e2 (paren level ")") in
  pp 0 fmt e

let pp_typed_symbol fmt (name,t) =
  fprintf fmt "%a %a" pp_simple_type t.Syntax.Annot.typ Rfsm.Ident.pp name 

