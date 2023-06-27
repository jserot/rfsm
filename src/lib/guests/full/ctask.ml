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

let pp_simple_type fmt t =
  let open Types in 
  match t with
  | TyConstr (c,[],_) -> fprintf fmt "%s" c
  | TyRecord (nm,_) -> fprintf fmt "%s" nm
  | _ -> raise (Unsupported_type (Some t))

let pp_expr fmt e = 
  let open Syntax in
  let paren level p = if level > 0 then p else "" in
  let cop_of op = match op with
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
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EChar c -> fprintf fmt "'%c'" c
  | EFapp (op,[e]) when op.[0] = '~' -> fprintf fmt "-%a" (pp (level+1)) e (* Unary "-" *)
  | EFapp (f,es) -> fprintf fmt "%s(%a)" f (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) es
  | EBinop (op,e1,e2) ->
       fprintf fmt "%s%a%s%a%s" (paren level "(") (pp (level+1)) e1 (cop_of op) (pp (level+1)) e2 (paren level ")")
  | ECon0 c -> fprintf fmt "%s" c
  | EIndexed (a,i) -> fprintf fmt "%s[%a]" a (pp level) i
  | ERanged (a,hi,lo) -> fprintf fmt "%s[%a:%a]" a (pp level) hi (pp level) lo (* Not strictly C *)
  | EArrExt vs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp level)) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" (pp level) e1 (pp level) e2 (pp level) e3
  | ECast (e,t) -> fprintf fmt "((%a)(%a))" pp_type_expr t (pp level) e
  | ERecordExt fs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," (pp_rfield level)) fs (* Not strictly C *)
  | ERecord (r,f) -> fprintf fmt "%s.%s" r f 
  and pp_rfield level fmt (n,v) = fprintf fmt "%s=%a" n (pp level) v in
  pp 0 fmt e

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

