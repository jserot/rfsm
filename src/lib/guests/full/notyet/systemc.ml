exception Unsupported_type of Syntax.Types.typ option
exception Unsupported_expr of Syntax.expr
                                  
open Format 

module Syntax = Syntax

let pp_type_expr fmt te = 
  let open Syntax in
  match te.Annot.desc with
  | TeConstr ("int",[],_) -> fprintf fmt "int" 
  | TeConstr ("bool",[],_) -> fprintf fmt "bool" 
  | TeConstr ("float",[],_) -> fprintf fmt "float" 
  | TeConstr ("char",[],_) -> fprintf fmt "char" 
  | TeConstr ("event",[],_) -> fprintf fmt "event" 
  | _ -> raise (Unsupported_type te.Annot.typ)
                    
let rec pp_type_decl_desc fmt td = 
  let open Syntax in
  let pp_rfield fmt (n,t) = fprintf fmt "%a %s; " pp_type_expr t n in
  match td with
  | TD_Enum (name,ctors) ->
     fprintf fmt "typedef enum { %a } %s;"
       (Rfsm.Misc.pp_list_h ~sep:"," pp_print_string) ctors
       name
  | TD_Record (name,fields) ->
     fprintf fmt "typedef struct { %a} %s;"
       (Rfsm.Misc.pp_list_h pp_rfield) fields
       name
  | TD_Alias (name,t) ->
     fprintf fmt "typedef %a %s;" pp_type_expr t name
and pp_type_decl fmt td = Format.fprintf fmt "%a@." pp_type_decl_desc td.Syntax.Annot.desc

let pp_size fmt sz = 
  let open Types in 
  match sz with
  | SzExpr1 (TiConst n) -> fprintf fmt "%d" n
  | SzExpr2 _ -> Rfsm.Misc.not_implemented "Ctask translation of 2D arrays"
  | _ -> fprintf fmt "" (* TO REFINE ? *)

let pp_type fmt t 
  let open Types in 
  match t with
    | TyConstr ("int",[],SzExpr1 (TiConst sz)) -> "sc_int<" ^ string_of_int sz ^ "> "
    | TyInt ("int",[], _) -> "int" 
    | TyConstr (c,[],_) -> fprintf fmt "%s" c
    | TyConstr ("float",[],_) -> if cfg.sc_double_float then "double" else "float"
    | TyRecord (nm,_) -> fprintf fmt "%s" nm
    | _ -> raise (Unsupported_type (Some t))

let pp_op fmt op = 
  match op with
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | "~-" -> "-" 
  | "~-." -> "-" 
  | op ->  op in

let pp_expr m fmt e = 
  let open Syntax in
  let rec pp level fmt e = match e.Annot.desc, e.Annot.typ with
  | EVar v, _ -> fprintf fmt "%a" pp_access v
  | EInt i, _ -> fprintf fmt "%d" i
  | EBool b, _ -> fprintf fmt "%b" b
  | EFloat f, _ -> fprintf fmt "%f" f
  | EChar c, _ -> fprintf fmt "'%c'" c
  | EFapp (("~-"|"~-."),[e]), _ -> "-" ^ "(" ^ string_of level e ^ ")"
  | EFapp (f,es), _ -> fprintf fmt "%s(%a)" f (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) es
  | EBinop (op,e1,e2), _ ->
       fprintf fmt "%s%a%a%a%s" (paren level "(") (pp (level+1)) e1 (pp_op op) (pp (level+1)) e2 (paren level ")")
  | ECon0 c, Some (TyConstr (t,_,_)) -> fprintf fmt "%s::%s" t c
  | EIndexed (a,i), _ -> fprintf fmt "%s[%a]" a (pp level) i
  | EIndexed (a,i), _ -> fprintf fmt "%a.range(%a,%a)" pp_access a (pp level) i (pp level) i
  | ERanged (a,hi,lo), _ -> fprintf fmt "%a.range(%a,%a)" pp_access a (pp level) hi (pp level) lo
  | EArrExt vs, _ -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) vs
  | ECond (e1,e2,e3), _ -> fprintf fmt "%a?%a:%a" (pp (level+1)) e1 (pp (level+1)) e2 (pp (level+1)) e3
  | ECast (e,t), _ -> fprintf fmt "((%a)(%a))" pp_type_expr t (pp level) e
  | ERecordExt fs, _ -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp_rfield level)) fs (* Not strictly C *)
  | ERecord (r,f), _ -> fprintf fmt "%a.repr.%s" pp_access r f 
  and pp_rfield level fmt (n,v) = fprintf fmt "%s=%a" n (pp level) v in
  and access id = if List.mem_assoc id (m.c_inps @ m.c_inouts) then id ^ ".read()" else id in
  and paren level p = if level > 0 then p else "" in
  pp 0 fmt e

  (* let rec string_of_value v = match v.Expr.v_desc, Types.real_type v.Expr.v_typ with
   *     Expr.Val_int i, _ -> string_of_int i
   *   | Expr.Val_float i, _-> string_of_float i
   *   | Expr.Val_bool i, _ -> string_of_bool i
   *   | Expr.Val_char c, _ -> string_of_char c
   *   | Expr.Val_enum c, Types.TyEnum (name, _) -> string_of_enum_value (Types.string_of_name name) c
   *   | Expr.Val_fn _, _ -> "<fun>"
   *   | Expr.Val_unknown, _ -> "<unknown>"
   *   | Expr.Val_none, _ -> "<none>"
   *   | Expr.Val_array vs, _ -> "{" ^ Utils.ListExt.to_string string_of_value "," (Array.to_list vs) ^ "}"
   *   | Expr.Val_record fs, Types.TyRecord (name, _) -> string_of_record_value (Types.string_of_name name) fs
   *   | _, _ -> Misc.fatal_error "Systemc.string_of_value" *)

let pp_typed_symbol fmt (name,t) =
  let open Types in 
  match t.Syntax.Annot.typ with
  | Some (TyConstr ("array", [t'], sz)) -> fprintf fmt "%a %s[%a]" pp_simple_type t' name pp_size sz
  | Some t -> fprintf fmt "%a %s" pp_simple_type t name 
  | None -> Rfsm.Misc.fatal_error "Ctask.pp_typed_symbol"

let pp_cst_decl fmt name t = 
  let open Types in 
  match t.Syntax.Annot.typ with
  | Some (TyConstr ("array",_,_)) -> fprintf fmt "extern %a" pp_typed_symbol (name,t)
  | Some _ -> fprintf fmt "%a" pp_typed_symbol (name,t)
  | None -> Rfsm.Misc.fatal_error "Ctask.pp_cst_decl"

let pp_cst_impl fmt name t v = 
  fprintf fmt "%a = %a" pp_typed_symbol (name,t) pp_expr v

let dump_record_type_defn fmt name fields = 
  let mk f sep fs = Utils.ListExt.to_string f sep fs in
  fprintf fmt "class %s {\n" name;
  fprintf fmt "public:\n";
  fprintf fmt "  struct { %s } repr;\n"
    (mk (function (n,t) -> string_of_type t ^ " " ^ n ^ ";") " " fields);
  fprintf fmt "  %s() { };\n" name;
  fprintf fmt "  %s(%s) { %s };\n"
    name
    (mk (function (n,t) -> string_of_type t ^ " " ^ n) ", " fields)
    (mk (function (n,_) -> "repr." ^ n ^ "=" ^ n ^ ";") " " fields);
  fprintf fmt "  inline bool friend operator == ( const %s& v1, const %s& v2) { return %s; }\n" name name
    (mk (function (n,_) -> "v1.repr." ^ n ^ "==v2.repr." ^ n) " && " fields);
  fprintf fmt "  inline %s& operator = (const %s& v) { repr = v.repr; return *this; }\n" name name;
  fprintf fmt "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %s& v) {\n" name;
  fprintf fmt "    os << \"{\" << %s << \"} \";\n"
    (mk (function (n,_) -> "\"" ^ n ^ "=\"" ^ " << v.repr." ^ n) " << \",\" << " fields); 
  fprintf fmt "    return os;\n";
  fprintf fmt "    }\n";
  fprintf fmt "  inline friend void sc_trace(sc_trace_file *tf, const %s& v, const std::string& n) {\n" name;
  List.iter
    (function (n,_) -> fprintf fmt "    sc_trace(tf,v.repr.%s, n+\".%s\");\n" n n)
    fields;
  fprintf fmt "  }\n";
  fprintf fmt "};\n"

let dump_enum_type_defn fmt name vs = 
  fprintf fmt "class %s {\n" name;
  fprintf fmt "public:\n";
  fprintf fmt "  enum values { %s };\n"
    (Utils.ListExt.to_string (function (v,i) -> v ^ "=" ^ string_of_int i) "," (List.mapi (fun i v -> v,i) vs));
  fprintf fmt "  int repr; // SystemC 2.3 does not allow tracing of enumerated values :(\n";
  fprintf fmt "  static const char* names[%d];\n" (List.length vs);
  fprintf fmt "  %s() { };\n" name;
  fprintf fmt "  %s(int r) { repr=r; };\n" name;
  fprintf fmt "  inline friend bool operator == ( const %s& v1, const %s& v2) { return v1.repr == v2.repr; }\n" name name;
  fprintf fmt "  inline %s& operator = (const %s& v) { repr = v.repr; return *this; }\n" name name;
  fprintf fmt "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %s& v) {\n" name;
  fprintf fmt "     os << names[v.repr];\n";
  fprintf fmt "     return os;\n";
  fprintf fmt "     }\n";
  fprintf fmt "  inline friend void sc_trace(sc_trace_file *tf, const %s& v, const std::string& n) {\n" name;
  fprintf fmt "     sc_trace(tf, v.repr, n);\n";
  fprintf fmt "     }\n";
  fprintf fmt "};\n\n"

  let dump_enum_type_names ocf = function
    | _ , Types.TyEnum (nm,cs) ->
       fprintf ocf "const char* %s::names[%d] = { %s };\n" 
         (Types.string_of_name nm)
         (List.length cs)
         (Utils.ListExt.to_string (function c -> "\"" ^ c ^ "\"") ", " cs)
    | _ -> ()

