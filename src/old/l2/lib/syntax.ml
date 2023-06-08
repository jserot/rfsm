module Types = Types

module Location = Msic.Location
module Annot = Msic.Annot

let print_types = ref false

let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.make ~loc:(mk_location l) x

type type_decl_desc =
  | TD_Enum of string * string list (* Name, constructors *)
and type_decl = (type_decl_desc,Types.typ) Annot.t
              
type type_expr_desc = 
  | TeConstr of string * type_expr list * int option (* name, args, size annotation *)
and type_expr = (type_expr_desc,Types.typ) Annot.t

type var_decl = (var_decl_desc,Types.typ) Annot.t
and var_decl_desc = string * type_expr  (* name, type *)
              
type expr_desc = 
  | EVar of string
  | EInt of int
  | EBool of bool
  | EBinop of string * expr * expr
  | EArr of string * expr (* a[i] *)
  | ECon0 of string (* Nullary value constructor *)
and expr = (expr_desc,Types.typ) Annot.t

type action_desc =
  | Assign of lhs * expr
and action = (action_desc,Types.typ) Annot.t

and lhs_desc =
  | LhsVar of string
  | LhsArrInd of string * expr (* a[i] *)
and lhs = (lhs_desc,Types.typ) Annot.t

let rec pp_type_expr_desc fmt te = 
  let open Format in
  let pp_sz fmt sz = match sz with
    | None -> pp_print_string fmt ""
    | Some s -> fprintf fmt "[%d]" s in
  match te with
  | TeConstr (c,[],sz) -> fprintf fmt "%s%a" c pp_sz sz
  | TeConstr (c,[t'],sz) -> fprintf fmt " %a %s%a" pp_type_expr t' c pp_sz sz
  | TeConstr (c,ts,sz) -> fprintf fmt " %a %s%a" (Msic.Misc.pp_list_h pp_type_expr) ts c pp_sz sz
and pp_type_expr fmt te = 
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_type_expr_desc te.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) te.Annot.typ
  else
    Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

let rec pp_expr_desc fmt e = 
  let open Format in
  match e with
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool i -> fprintf fmt "%b" i
  | EBinop (op,e1,e2) -> fprintf fmt "%a%s%a" pp_expr e1 op pp_expr e2
  | EArr (x,i) -> fprintf fmt "%s[%a]" x pp_expr i
  | ECon0 c -> fprintf fmt "%s" c
and pp_expr fmt e =
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_expr_desc e.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) e.Annot.typ
  else
    pp_expr_desc fmt e.Annot.desc

let rec pp_lhs_desc fmt l = 
  let open Format in
  match l with
  | LhsVar x -> fprintf fmt "%s" x
  | LhsArrInd (a,i) -> fprintf fmt "%s[%a]" a pp_expr i
and pp_lhs fmt l =
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_lhs_desc l.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) l.Annot.typ
  else
    Format.fprintf fmt "%a" pp_lhs_desc l.Annot.desc

let rec pp_action_desc fmt a = 
  let open Format in
  match a with
  | Assign (lhs,e) -> fprintf fmt "%a:=%a" pp_lhs lhs pp_expr e
and pp_action fmt l =
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_action_desc l.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) l.Annot.typ
  else
    Format.fprintf fmt "%a" pp_action_desc l.Annot.desc 

let rec pp_type_decl_desc fmt td = 
  let open Format in
  match td with
  | TD_Enum (name,ctors) -> fprintf fmt "(\"%s\", enum { %a })" name (Msic.Misc.pp_list_h ~sep:"," pp_print_string) ctors
and pp_type_decl fmt td =
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_type_decl_desc td.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) td.Annot.typ
  else
    Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

let rec pp_var_decl_desc fmt d = 
  let open Format in
  match d with
  |  (x,te) -> fprintf fmt "%s: %a" x pp_type_expr te
and pp_var_decl fmt d =
  if !print_types then
    Format.fprintf fmt "%a:%a" pp_var_decl_desc d.Annot.desc (Msic.Misc.pp_opt Types.pp_typ) d.Annot.typ
  else
    Format.fprintf fmt "%a" pp_var_decl_desc d.Annot.desc 
