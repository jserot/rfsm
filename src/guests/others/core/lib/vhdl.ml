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

let vhdl_type_of t =
  let open Rfsm.Vhdl_types in
  match t with 
    | Types.TyConstr ("int",[]) -> Integer None
    | Types.TyConstr ("event",[]) -> Std_logic
    | Types.TyConstr ("bool",[]) -> if cfg.vhdl_bool_as_bool then Boolean else Std_logic
    | _ -> raise (Unsupported_type t)

let pp_op fmt op = 
  fprintf fmt "%s"
  (match op.Rfsm.Ident.id with
    "=" -> " = "
  | "!=" -> " /= "
  | "%" -> " mod "
  | "&" -> " and " 
  | "|" -> " or " 
  | "^" -> " xor " 
  | op ->  op)

let pp_ident = Rfsm.Ident.pp 

let rec pp_expr fmt e = 
  let open Rfsm.Vhdl_types in
  let open Format in
  let rec pp level fmt e = match e.Syntax.Annot.desc with
    | Syntax.EVar n ->  pp_ident fmt n
    | Syntax.EInt n -> fprintf fmt "%d" n
    | Syntax.EBool c -> pp_bool fmt c
    | Syntax.EBinop (op,e1,e2) -> 
       fprintf fmt "%s%a%a%a%s" (paren level "(") (pp (level+1)) e1 pp_op op (pp (level+1)) e2 (paren level ")")
    | Syntax.ECon0 c -> fprintf fmt "%s%a" Rfsm.Vhdl_types.cfg.vhdl_enum_prefix pp_ident c
  and paren level p = if level > 0 then p else "" in
  pp 0 fmt e

and pp_bool fmt b = match Rfsm.Vhdl_types.cfg.vhdl_bool_as_bool, b with
  | true, _ -> fprintf fmt "%b" b
  | false, true -> fprintf fmt "'1'"
  | false, false -> fprintf fmt "'0'"
                  
let rec pp_lhs_desc fmt l =
  match l with 
  | Syntax.LhsVar v -> fprintf fmt "%a" pp_ident v
and pp_lhs fmt l = pp_lhs_desc fmt l.Rfsm.Annot.desc

let pp_type_decl fmt td = ()
let pp_type_fns_intf fmt td = ()
let pp_type_fns_impl fmt td = ()
let pp_array_type_decl fmt te = ()
  (* There's no type, function nor constant declaration for the Core guest language *)

let pp_value fmt (v,_) = 
  let open Format in
  let open Value in
  let pp_v fmt v = match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_enum c -> fprintf fmt "%s%s" Rfsm.Vhdl_types.cfg.vhdl_enum_prefix c 
  | _ -> raise (Unsupported_value v) in
 pp_v fmt v

let allowed_shared_type ty = true
