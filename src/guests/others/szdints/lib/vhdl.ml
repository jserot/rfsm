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
exception Illegal_cast of Syntax.expr

let rec vhdl_type_of t =
  let open Rfsm.Vhdl_types in
  match Types.real_type t with 
    | Types.TyConstr ("event",[],_) -> Std_logic
    | Types.TyConstr ("bool",[],_) -> if cfg.vhdl_bool_as_bool then Boolean else Std_logic
    | Types.TyConstr ("bit",[],_) -> Std_logic
    | Types.TyConstr ("float",[],_) -> Real
    | Types.TyConstr ("char",[],_) -> Char
    | Types.TyConstr ("int",[],[sz]) ->
       if cfg.vhdl_use_numeric_std then Unsigned sz
       else Integer (Some (0, 1 lsl sz - 1))
    | Types.TyConstr ("int",[],[lo;hi]) ->
       (* if cfg.vhdl_use_numeric_std then
        *   if lo < 0 then Signed (Rfsm.Bits.bit_size (max (-lo) hi)) else Unsigned (Rfsm.Bits.bit_size hi)
        * else *)
         Integer (Some (lo,hi))
    | TyConstr ("int",[],sz) ->
       Integer None
    | TyConstr (c,[],_) -> Enum (c,[]) (* TO FIX - add ctors ? *)
    | TyConstr ("array",[t'],[sz]) -> Array (sz, vhdl_type_of t')
    | TyRecord (nm, fs) ->
       Record (nm, List.map (function (n,ty) -> n, vhdl_type_of ty) fs)
    | _ -> raise (Unsupported_type t)

let pp_op fmt op = 
  fprintf fmt "%s"
  (match op.Rfsm.Ident.id with
    "=" -> " = "
  | "!=" -> " /= "
  | "%" -> " mod "
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | "&" -> " and " 
  | "|" -> " or " 
  | "^" -> " xor " 
  | op ->  op)

let pp_ident = Rfsm.Ident.pp 

let rec pp_expr fmt e = 
  let open Rfsm.Vhdl_types in
  let open Format in
  let rec pp level fmt e = match e.Syntax.Annot.desc, vhdl_type_of e.Syntax.Annot.typ with
    | Syntax.EVar n, _ ->  pp_ident fmt n
    | Syntax.EInt n, Unsigned 1 -> fprintf fmt "'%d'" n
    | Syntax.EInt n, Unsigned s -> fprintf fmt "to_unsigned(%d,%d)" n s
    | Syntax.EInt n, Signed 1 -> fprintf fmt "'%d'" n
    | Syntax.EInt n, Signed s -> fprintf fmt "to_signed(%d,%d)" n s
    | Syntax.EInt n, Boolean -> pp_bool fmt (n > 0)
    | Syntax.EInt n, Std_logic -> pp_bool fmt (n > 0)
    | Syntax.EInt n, _ -> fprintf fmt "%d" n
    | Syntax.EBool c, _ -> pp_bool fmt c
    | Syntax.EFloat c, _ -> pp_float fmt c
    | Syntax.EChar c, _ -> pp_char fmt c
    | Syntax.EFapp (op,[e]), _ when List.mem op.Rfsm.Ident.id ["~-";"~-."]-> fprintf fmt "-%a" (pp (level+1)) e
    | Syntax.EFapp (f,es), _ -> fprintf fmt "%a(%a)" pp_ident f (Rfsm.Ext.List.pp_h ~sep:"," (pp level)) es
    | Syntax.EBinop (Rfsm.Ident.{id=">>";_},e1,e2), _ ->
       fprintf fmt "shift_right(%a,%a)" (pp (level+1)) e1 pp_int_expr e2
    | Syntax.EBinop (Rfsm.Ident.{id="<<";_},e1,e2), _ -> 
       fprintf fmt "shift_left(%a,%a)" (pp (level+1)) e1 pp_int_expr e2
    | Syntax.EBinop (op,e1,e2), _ -> 
       begin match op.Rfsm.Ident.id, vhdl_type_of e1.Rfsm.Annot.typ, vhdl_type_of e2.Rfsm.Annot.typ with
       | "*", Signed _, _
       | "*", Unsigned _, _
       | "*", _, Unsigned _
       | "*", _, Signed _ ->  fprintf fmt "mul(%a,%a)" (pp (level+1)) e1 (pp (level+1)) e2
       | _, _, _ -> fprintf fmt "%s%a%a%a%s" (paren level "(") (pp (level+1)) e1 pp_op op (pp (level+1)) e2 (paren level ")")
       end
    | Syntax.ECon0 c, _ -> pp_enum fmt c
    | Syntax.EIndexed (a,i), _ -> fprintf fmt "%a(%a)" pp_ident a (pp level) i
    | Syntax.ERanged (a,hi,lo), _ -> fprintf fmt "%a(%a)" pp_ident a pp_range (hi,lo)
    | Syntax.ECond (e1,e2,e3), _ -> fprintf fmt "cond(%a,%a,%a)" (pp (level+1)) e1 (pp (level+1)) e2 (pp (level+1)) e3
    | Syntax.EArrExt vs, _ -> fprintf fmt "(%a)" (Rfsm.Ext.List.pp_h ~sep:"," (pp level)) vs
    | Syntax.ERecord (r,f), _ -> fprintf fmt "%a.%s" pp_ident r f 
    | Syntax.ERecordExt vs, _ -> fprintf fmt "(%a)" (Rfsm.Ext.List.pp_h ~sep:"," (pp level)) (List.map snd vs)
    | Syntax.ECast (e,te), _ -> pp_cast fmt (e,te)
  and paren level p = if level > 0 then p else "" in
  pp 0 fmt e

and pp_int_expr fmt e = match e.Rfsm.Annot.desc, vhdl_type_of e.Rfsm.Annot.typ with
    Syntax.EInt n, _ -> fprintf fmt "%d" n
  | _, Integer _ -> pp_expr fmt e
  | _, _ -> fprintf fmt "to_integer(%a)" pp_expr e

and pp_float fmt x = fprintf fmt "%E" x

and pp_bool fmt b = match Rfsm.Vhdl_types.cfg.vhdl_bool_as_bool, b with
  | true, _ -> fprintf fmt "%b" b
  | false, true -> fprintf fmt "'1'"
  | false, false -> fprintf fmt "'0'"
                  
and pp_char fmt c = fprintf fmt "'%c'" c

and pp_enum fmt id = fprintf fmt "%s%a" Rfsm.Vhdl_types.cfg.vhdl_enum_prefix pp_ident id

and pp_range fmt (hi,lo) =
  match hi.Rfsm.Annot.desc, lo.Rfsm.Annot.desc with
  | Syntax.EInt n1, Syntax.EInt n2 when n1=n2 -> fprintf fmt "%a" pp_int_expr hi  (* Special case *)
  | Syntax.EVar v1, Syntax.EVar v2 when v1=v2 -> fprintf fmt "%a" pp_int_expr hi  (* Special case *)
  | _, _ -> fprintf fmt "%a downto %a" pp_int_expr hi pp_int_expr lo

and pp_cast fmt (e,te) = 
  let open Format in
  match vhdl_type_of e.Rfsm.Annot.typ, vhdl_type_of te.Rfsm.Annot.typ with
  | Integer _, Unsigned n -> fprintf fmt "conv_unsigned(%a,%d)" pp_expr e n
  | Signed n, Unsigned n' when n=n' -> fprintf fmt "conv_unsigned(%a,%d)" pp_expr e n
  | Boolean, Unsigned n ->  fprintf fmt "conv_unsigned(%a,%d)" pp_expr e n
  | Unsigned n', Unsigned n when n<>n' ->  fprintf fmt "resize(%a,%d)" pp_expr e n
  | Integer _, Signed n -> fprintf fmt "conv_signed(%a,%d)" pp_expr e n
  | Signed n, Signed n' when n=n' -> fprintf fmt "conv_signed(%a,%d)" pp_expr e n
  | Boolean, Signed n ->  fprintf fmt "conv_signed(%a,%d)" pp_expr e n
  | Signed n', Signed n when n<>n' -> fprintf fmt "resize(%a,%d)" pp_expr e n
  | Integer _, Boolean -> fprintf fmt "to_bool(%a)" pp_expr e
  | Unsigned _, Boolean -> fprintf fmt "to_bool(%a)" pp_expr e
  | Unsigned _, Integer _ -> fprintf fmt "to_integer(%a)" pp_expr e
  | Signed _, Boolean -> fprintf fmt "to_bool(%a)" pp_expr e
  | Signed _, Integer _ -> fprintf fmt "to_integer(%a)" pp_expr e
  | Integer _, Integer _ -> fprintf fmt "%a" pp_expr e
  | Integer _, Char -> fprintf fmt "to_char(%a)" pp_expr e
  | Unsigned _, Char -> fprintf fmt "to_char(%a)" pp_expr e
  | Char, Integer _ -> fprintf fmt "to_integer(%a)" pp_expr e
  | Char, Unsigned n -> fprintf fmt "conv_unsigned(%a,%d)" pp_expr e n
  | Integer _, Std_logic ->  fprintf fmt "to_stdlogic(%a)" pp_expr e
  | t, t' when t=t' -> pp_expr fmt e
  | _, _ -> raise (Illegal_cast e)

let rec pp_lval_desc fmt l =
  match l with 
  | Syntax.LvalVar v -> fprintf fmt "%a" pp_ident v
  | Syntax.LvalIndex (a,i) -> fprintf fmt "%a(%a)" pp_ident a pp_expr i
  | Syntax.LvalRange (a,hi,lo) -> fprintf fmt "%a(%a)" pp_ident a pp_range (hi,lo)
  | Syntax.LvalRField (r,f) -> fprintf fmt "%a.%s" pp_ident r f
and pp_lval fmt l = pp_lval_desc fmt l.Rfsm.Annot.desc

let rec pp_value fmt (v,ty) =
  let open Format in
  let open Rfsm.Vhdl_types in
  match v, ty with
    Value.Val_int i, Unsigned n -> fprintf fmt "to_unsigned(%d,%d)" i n
  | Value.Val_int i, Signed n -> fprintf fmt "to_signed(%d,%d)" i n
  | Value.Val_int i, Std_logic -> fprintf fmt "'%d'" i
  | Value.Val_int i, Integer _ -> fprintf fmt "%d" i
  | Value.Val_int i, Boolean -> pp_bool fmt (i > 0)
  | Value.Val_int i, Real -> fprintf fmt "%d.0" i
  | Value.Val_int i, _ -> fprintf fmt "%d" i
  | Value.Val_float f, _ -> pp_float fmt f
  | Value.Val_char f, _ -> pp_char fmt f
  | Value.Val_bool b, _ -> pp_bool fmt b
  | Value.Val_enum s, _ -> fprintf fmt "%s%s" Rfsm.Vhdl_types.cfg.vhdl_enum_prefix s
  | Value.Val_fn _, _ -> Rfsm.Misc.not_implemented "Guest.VHDL: translation of function value"
  | Value.Val_unknown, _ -> fprintf fmt "<unknown>"
  | Value.Val_array vs, Array (n,t') ->
     let pp_value' fmt v = pp_value fmt (v,t') in
     fprintf fmt "(%a)" (Rfsm.Ext.List.pp_h ~sep:"," pp_value') (Array.to_list vs)
  | Value.Val_record fs, Record (_,ts) ->
     let pp_value' fmt (v,t) = pp_value fmt (v,t) in
     fprintf fmt "(%a)"
       (Rfsm.Ext.List.pp_h ~sep:"," pp_value') (List.map2 (fun (_,v) (_,t) -> v,t) fs ts)
  | _, _ ->
     Rfsm.Misc.fatal_error "Guest.Vhdl.pp_value"

let pp_typ fmt ~type_mark t = Rfsm.Vhdl_types.pp ~type_mark fmt (vhdl_type_of t)
let pp_type_expr fmt ~type_mark te = pp_typ fmt ~type_mark te.Rfsm.Annot.typ
let pp_full_type_expr fmt t = pp_type_expr fmt ~type_mark:Rfsm.Vhdl_types.TM_Full t

let pp_enum_type_defn fmt (name,ctors) = 
  let pp_ctor fmt c = fprintf fmt "%s%s" Rfsm.Vhdl_types.cfg.vhdl_enum_prefix c in
  fprintf fmt "  type %a is (%a);\n" pp_ident name (Rfsm.Ext.List.pp_h ~sep:"," pp_ctor) ctors

let pp_record_type_defn fmt (name,fields) = 
  fprintf fmt "  type %a is record\n" pp_ident name;
  List.iter
    (function (n,t) -> fprintf fmt "    %s: %a;\n" n pp_full_type_expr t)
    fields;
  fprintf fmt "  end record;\n"

let rec pp_type_decl_desc fmt td = 
  match td with
  | Syntax.TD_Enum (name,ctors) -> pp_enum_type_defn fmt (name,ctors)
  | Syntax.TD_Record (name,fields) -> pp_record_type_defn fmt (name,fields)
  | Syntax.TD_Alias (name,t) ->
     fprintf fmt "  %s %a is %a;" (if Syntax.is_array_type t then "type" else "subtype") pp_ident name pp_full_type_expr t
and pp_type_decl fmt td = Format.fprintf fmt "%a@." pp_type_decl_desc td.Syntax.Annot.desc

let pp_type_fns_intf fmt td =
  match td.Rfsm.Annot.desc with
  | Syntax.TD_Enum (name,_)
  | Syntax.TD_Record (name,_) ->
     fprintf fmt "  function cond(e1: boolean; e2: %a; e3: %a) return %a;\n"
       pp_ident name pp_ident name pp_ident name
  | Syntax.TD_Alias (name,t) -> ()

let pp_type_fns_impl fmt td =
  match td.Rfsm.Annot.desc with
  | Syntax.TD_Enum (name,_)
  | Syntax.TD_Record (name,_) ->
       fprintf fmt "function cond(e1: boolean; e2: %a; e3: %a) return %a is\n"
         pp_ident name pp_ident name pp_ident name;
       fprintf fmt "  begin\n";
       fprintf fmt "    if e1 then return e2; else return e3; end if;\n";
       fprintf fmt "  end;\n"
  | Syntax.TD_Alias (name,t) -> ()

let pp_array_type_decl fmt te =
  match te.Rfsm.Annot.typ with
  | Types.TyConstr ("array",[t'],[sz]) as t -> 
     let open Rfsm.Vhdl_types in
     let pp_typ = pp_typ ~type_mark:TM_Abbr in
     fprintf fmt "  type %a is array (0 to %d) of %a;\n" pp_typ t (sz-1) pp_typ t'
  | _ -> ()

let allowed_shared_type ty = 
  match ty with 
    | Types.TyConstr ("int",[],_)
    | Types.TyConstr ("bit",[],_)
    | Types.TyConstr ("bool",[],_)
    | Types.TyConstr ("float",[],_)
    | Types.TyConstr ("char",[],_)
    | Types.TyRecord _ -> true
    | _ -> false
