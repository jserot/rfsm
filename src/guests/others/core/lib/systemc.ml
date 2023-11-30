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

open Format 

module Syntax = Syntax
module Static = Static

type value = Value.t

exception Unsupported_type of Syntax.Types.typ
exception Unsupported_expr of Syntax.expr
exception Unsupported_value of Value.t

let pp_type_expr fmt te = 
  let open Syntax in
  match te.Annot.desc with
  | TeConstr "event" -> fprintf fmt "bool"
  | TeConstr c -> fprintf fmt "%s" c 

let pp_typ fmt t =
  let open Types in 
  match t with
  | TyConstr ("event",[]) -> fprintf fmt "bool"
  | TyConstr (c,[]) -> fprintf fmt "%s" c
  | _ -> raise (Unsupported_type t)

let pp_op fmt op = 
  fprintf fmt "%s"
  (match op.Rfsm.Ident.id with
    "=" -> "=="
  | "~-" -> "-" 
  | op ->  op)

let pp_expr fmt e = 
  let rec pp level fmt e = match e.Syntax.Annot.desc, e.Syntax.Annot.typ with
  | Syntax.EVar v, _ -> fprintf fmt "%a" pp_access v
  | Syntax.EInt i, _ -> fprintf fmt "%d" i
  | Syntax.EBool b, _ -> fprintf fmt "%b" b
  | Syntax.EBinop (op,e1,e2), _ ->
       fprintf fmt "%s%a%a%a%s" (paren level "(") (pp (level+1)) e1 pp_op op (pp (level+1)) e2 (paren level ")")
  | Syntax.ECon0 c, Types.TyConstr (t,_) -> fprintf fmt "%s::%a" t Rfsm.Ident.pp c
  | _, _ -> raise (Unsupported_expr e)
  and pp_access fmt id =
    let open Rfsm.Ident in
    match id.scope with
    | Global -> fprintf fmt "%s.read()" id.id
    | Local -> fprintf fmt "%s" id.id
  and paren level p = if level > 0 then p else "" in
  pp 0 fmt e

let rec pp_lval_desc fmt l =
  match l with 
  | Syntax.LvalVar v -> Format.fprintf fmt "%a" Rfsm.Ident.pp v
and pp_lval fmt l = pp_lval_desc fmt l.Rfsm.Annot.desc

let pp_value fmt v = 
  let open Format in
  let open Value in
  let pp_v fmt v = match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_enum c -> fprintf fmt "%s::%s" "<type>" c  (* TO FIX *)
  | _ -> raise (Unsupported_value v) in
 pp_v fmt v

let pp_typed_symbol fmt (name,t) =
  fprintf fmt "%a %a" pp_typ t.Syntax.Annot.typ Rfsm.Ident.pp name 

let pp_type_decl fmt td = ()
  (* There's no type declaration for the Core guest language *)

and pp_type_impl fmt td = ()
  (* There's no type declaration for the Core guest language *)

