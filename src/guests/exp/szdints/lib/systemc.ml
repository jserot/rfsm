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

let float_print_format = format_of_string "%.8f"

let pp_type_expr fmt te = 
  let open Syntax in
  let open Rfsm.Ident in
  match te.Annot.desc with
  | TeConstr ({id="int";_},[],[sz]) -> fprintf fmt "sc_uint<%d>" sz
  | TeConstr ({id="int";_},[],_) -> fprintf fmt "int" 
  | TeConstr ({id="bit";_},[],_) -> fprintf fmt "sc_uint<1>" 
  | TeConstr ({id="bool";_},[],_) -> fprintf fmt "bool" 
  | TeConstr ({id="float";_},[],_) -> fprintf fmt "%s" (if Rfsm.Systemc.cfg.Rfsm.Systemc.sc_double_float then "double" else "float")
  | TeConstr ({id="char";_},[],_) -> fprintf fmt "char" 
  | TeConstr ({id="event";_},[],_) -> fprintf fmt "bool" 
      (* Note: events are implemented as boolean signals because it is not possible to wait on multiple [sc_event]s 
         and telling afterwards which one occurred in SystemC 2.3.0 :-( *)
  | TeConstr ({id=c;_},[],_) -> fprintf fmt "%s" c  (* Enums *)
  | _ -> raise (Unsupported_type te.Annot.typ)
                    
let pp_typ fmt t =
  let open Types in 
  let open Format in 
  match t with
    | TyConstr ("int",[],[sz]) -> fprintf fmt "sc_uint<%d>" sz
    | TyConstr ("int",[], _) -> fprintf fmt "int" 
    | TyConstr ("bit",[], _) -> fprintf fmt "sc_uint<1>" 
    | TyConstr ("float",[],_) -> fprintf fmt "%s" (if Rfsm.Systemc.cfg.Rfsm.Systemc.sc_double_float then "double" else "float")
    | TyConstr ("event",[],_) -> fprintf fmt "bool"
      (* Note: events are implemented as boolean signals because it is not possible to wait on multiple [sc_event]s 
         and telling afterwards which one occurred in SystemC 2.3.0 :-( *)
    | TyConstr (c,[],_) -> fprintf fmt "%s" c
    | TyRecord (nm,_) -> fprintf fmt "%s" nm
    | _ -> raise (Unsupported_type t)

let pp_op fmt op = 
  fprintf fmt "%s"
  (match op.Rfsm.Ident.id with
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | "~-" -> "-" 
  | "~-." -> "-" 
  | op ->  op)

let pp_ident = Rfsm.Ident.pp 

let pp_expr fmt e = 
  let rec pp level fmt e = match e.Syntax.Annot.desc, e.Syntax.Annot.typ with
  | Syntax.EVar v, _ -> fprintf fmt "%a" pp_access v
  | Syntax.EInt i, _ -> fprintf fmt "%d" i
  | Syntax.EBool b, _ -> fprintf fmt "%b" b
  | Syntax.EFloat f, _ -> fprintf fmt float_print_format f
  | Syntax.EChar c, _ -> fprintf fmt "'%c'" c
  | Syntax.EFapp (op,[e]), _ when List.mem op.Rfsm.Ident.id ["~-";"~-."]-> fprintf fmt "-%a" (pp (level+1)) e
  | Syntax.EFapp (f,es), _ -> fprintf fmt "%a(%a)" pp_ident f (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) es
  | Syntax.EBinop (op,e1,e2), _ ->
       fprintf fmt "%s%a%a%a%s" (paren level "(") (pp (level+1)) e1 pp_op op (pp (level+1)) e2 (paren level ")")
  | Syntax.ECon0 c, Types.TyConstr (t,_,_) -> fprintf fmt "%s::%a" t pp_ident c
  | Syntax.EIndexed (a,i), _ -> fprintf fmt "%a[%a]" pp_access a (pp level) i
  | Syntax.ERanged (a,hi,lo), _ -> fprintf fmt "%a.range(%a,%a)" pp_access a (pp level) hi (pp level) lo
  | Syntax.EArrExt vs, _ -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) vs
  | Syntax.ECond (e1,e2,e3), _ -> fprintf fmt "%a?%a:%a" (pp (level+1)) e1 (pp (level+1)) e2 (pp (level+1)) e3
  | Syntax.ECast (e,t), _ -> fprintf fmt "((%a)(%a))" pp_type_expr t (pp level) e
  | Syntax.ERecordExt fs, Types.TyRecord (name,_) ->
     fprintf fmt "%s(%a)" name (Rfsm.Misc.pp_list_h ~sep:"," (pp_rfield_value level)) fs 
  | Syntax.ERecord (r,f), _ -> fprintf fmt "%a.repr.%s" pp_access r f 
  | _, _ -> raise (Unsupported_expr e)
  and pp_rfield_value level fmt (_,v) = fprintf fmt "%a" (pp level) v 
  and pp_access fmt id =
    let open Rfsm.Ident in
    match id.scope with
    | Global -> fprintf fmt "%s.read()" id.id
    | Local -> fprintf fmt "%s" id.id
  and paren level p = if level > 0 then p else "" in
  pp 0 fmt e

let rec pp_lhs_desc fmt l =
  match l with 
  | Syntax.LhsVar v -> Format.fprintf fmt "%a" pp_ident v
  | Syntax.LhsIndex (a,i) -> Format.fprintf fmt "%a[%a]" pp_ident a pp_expr i
  | Syntax.LhsRange (a,hi,lo) -> Format.fprintf fmt "%a.range(%a,%a)" pp_ident a pp_expr hi pp_expr lo
  | Syntax.LhsRField (r,f) -> Format.fprintf fmt "%a.repr.%s" pp_ident r f
and pp_lhs fmt l = pp_lhs_desc fmt l.Rfsm.Annot.desc

let pp_value fmt v = 
  let open Format in
  let open Value in
  let rec pp_v fmt v = match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_float v -> fprintf fmt float_print_format v
  | Val_char c -> fprintf fmt "'%c'" c
  | Val_array vs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," pp_v) (Array.to_list vs)
  | Val_enum c -> fprintf fmt "%s::%s" "<type>" c  (* TO FIX *)
  | Val_record fs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:";" pp_rfield) fs
  | _ -> raise (Unsupported_value v)
 and pp_rfield fmt (f,v) = fprintf fmt "%s=%a" f pp_v v in
 pp_v fmt v

let pp_typed_symbol fmt (name,t) =
  match t.Syntax.Annot.typ with
  | Types.TyConstr ("array", [t'], [sz]) -> fprintf fmt "%a %a[%d]" pp_typ t' pp_ident name sz
  | t -> fprintf fmt "%a %a" pp_typ t pp_ident name 

let pp_record_type_defn fmt (name,fields) = 
  let pp_typed_name fmt (n,t) = fprintf fmt "%a" pp_typed_symbol (Rfsm.Ident.mk n,t) in
  let pp_rfield fmt (n,t) = fprintf fmt "%a;" pp_typed_symbol (Rfsm.Ident.mk n,t) in
  fprintf fmt "class %a {\n" pp_ident name;
  fprintf fmt "public:\n";
  fprintf fmt "  struct { %a } repr;\n" (Rfsm.Misc.pp_list_h ~sep:" " pp_rfield) fields;
  fprintf fmt "  %a() { };\n" pp_ident name;
  fprintf fmt "  %a(%a) { %a };\n"
    pp_ident name
    (Rfsm.Misc.pp_list_h ~sep:"," pp_typed_name) fields
    (Rfsm.Misc.pp_list_h ~sep:" " (fun fmt (n,_) -> fprintf fmt "repr.%s = %s;" n n)) fields; 
  fprintf fmt "  inline bool friend operator == ( const %a& v1, const %a& v2) { return %a; }\n"
    pp_ident name
    pp_ident name
    (Rfsm.Misc.pp_list_h ~sep:" && " (fun fmt (n,_) -> fprintf fmt "v1.repr.%s==v2.repr.%s" n n)) fields;
  fprintf fmt "  inline %a& operator = (const %a& v) { repr = v.repr; return *this; }\n" pp_ident name pp_ident name;
  fprintf fmt "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %a& v) {\n" pp_ident name;
  fprintf fmt "    os << \"{\" << %a << \"} \";\n"
    (Rfsm.Misc.pp_list_h ~sep:" << \",\" << " (fun fmt (n,_) -> fprintf fmt  "\"%s=\" << v.repr.%s" n n)) fields; 
  fprintf fmt "    return os;\n";
  fprintf fmt "    }\n";
  fprintf fmt "  inline friend void sc_trace(sc_trace_file *tf, const %a& v, const std::string& n) {\n" pp_ident name;
  List.iter
    (function (n,_) -> fprintf fmt "    sc_trace(tf,v.repr.%s, n+\".%s\");\n" n n)
    fields;
  fprintf fmt "  }\n";
  fprintf fmt "};\n"

let pp_enum_type_defn fmt (name,vs) = 
  fprintf fmt "class %a {\n" pp_ident name;
  fprintf fmt "public:\n";
  fprintf fmt "  enum values { %a };\n"
    (Rfsm.Misc.pp_list_h ~sep:"," (fun fmt (v,i) -> fprintf fmt "%s=%d" v i)) (List.mapi (fun i v -> v,i) vs);
  fprintf fmt "  int repr; // SystemC 2.3 does not allow tracing of enumerated values :(\n";
  fprintf fmt "  static const char* names[%d];\n" (List.length vs);
  fprintf fmt "  %a() { };\n" pp_ident name;
  fprintf fmt "  %a(int r) { repr=r; };\n" pp_ident name;
  fprintf fmt "  inline friend bool operator == ( const %a& v1, const %a& v2) { return v1.repr == v2.repr; }\n" pp_ident name pp_ident name;
  fprintf fmt "  inline %a& operator = (const %a& v) { repr = v.repr; return *this; }\n" pp_ident name pp_ident name;
  fprintf fmt "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %a& v) {\n" pp_ident name;
  fprintf fmt "     os << names[v.repr];\n";
  fprintf fmt "     return os;\n";
  fprintf fmt "     }\n";
  fprintf fmt "  inline friend void sc_trace(sc_trace_file *tf, const %a& v, const std::string& n) {\n" pp_ident name;
  fprintf fmt "     sc_trace(tf, v.repr, n);\n";
  fprintf fmt "     }\n";
  fprintf fmt "};\n\n"

let rec pp_type_decl_desc fmt td = 
  match td with
  | Syntax.TD_Enum (name,ctors) -> pp_enum_type_defn fmt (name,ctors)
  | Syntax.TD_Record (name,fields) -> pp_record_type_defn fmt (name,fields)
  | Syntax.TD_Alias (name,t) -> fprintf fmt "typedef %a %a;" pp_type_expr t pp_ident name
and pp_type_decl fmt td = Format.fprintf fmt "%a@." pp_type_decl_desc td.Syntax.Annot.desc

let rec pp_type_impl_desc fmt td =
  let pp_ctor fmt c = Format.fprintf fmt "\"%s\"" c in
  match td with
  | Syntax.TD_Enum (name,ctors) ->
       Format.fprintf fmt "const char* %a::names[%d] = { %a };\n" 
         pp_ident name
         (List.length ctors)
         (Rfsm.Misc.pp_list_h ~sep:", " pp_ctor) ctors
    | _ -> ()
and pp_type_impl fmt td = Format.fprintf fmt "%a@." pp_type_impl_desc td.Syntax.Annot.desc

