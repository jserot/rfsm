module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
             
let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.make ~loc:(mk_location l) x

type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc = 
  | TeConstr of string * type_expr list * int option (* name, args, size annotation *)

type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of string
  | EInt of int
  | EBool of bool
  | EBinop of string * expr * expr
  | EArr of string * expr (* a[i] *)

let rec pp_expr_desc ~with_type fmt e = 
  let open Format in
  match e with
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EBinop (op,e1,e2) -> fprintf fmt "%a%s%a" (pp_expr ~with_type) e1 op (pp_expr ~with_type) e2
  | EArr (a,i) -> fprintf fmt "%s[%a]" a (pp_expr ~with_type) i
and pp_expr ?(with_type=false) fmt e =
  pp_expr_desc ~with_type fmt e.Annot.desc;
  if with_type then Format.fprintf fmt "{:%a}" (Rfsm.Misc.pp_opt Types.pp_typ) e.Annot.typ

type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of string
  | LhsArrInd of string * expr (* a[i] *)

let rec pp_lhs_desc ?(with_type=false) fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%s" v
  | LhsArrInd (a,i) -> Format.fprintf fmt "%s[%a]" a (pp_expr ~with_type) i
and pp_lhs ?(with_type=false) fmt l =
  pp_lhs_desc ~with_type fmt l.Annot.desc;
  if with_type then Format.fprintf fmt "{:%a}" (Rfsm.Misc.pp_opt Types.pp_typ) l.Annot.typ

let mk_simple_lhs v = Annot.make (LhsVar v)
let lhs_name l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsArrInd (a,_) -> a 

let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | _ -> []

let vars_of_lhs l = match l.Annot.desc with
  | LhsVar v -> [v]
  | LhsArrInd (a,i) -> a :: vars_of_expr i 

let subst_var phi v = Rfsm.Misc.subst_id phi v
                    
let rec subst_expr phi e =
  match e.Annot.desc with
  | EVar v -> { e with Annot.desc = EVar (subst_var phi v) }
  | EBinop (op,e1,e2) -> { e with Annot.desc = EBinop (op, subst_expr phi e1, subst_expr phi e2) }
  | EArr (a,i) -> { e with Annot.desc = EArr (subst_var phi a, subst_expr phi i) }
  | _ -> e

let subst_lhs phi l = 
  match l.Annot.desc with
  | LhsVar v -> { l with Annot.desc = LhsVar (subst_var phi v) }
  | LhsArrInd (a,i) -> { l with Annot.desc = LhsArrInd (subst_var phi a, subst_expr phi i) } 

let vcd_name lhs =
  match lhs.Annot.desc with
  | LhsVar v -> v
  | LhsArrInd (a,i) -> a ^ "." ^ Rfsm.Misc.to_string pp_expr i (* Note: syntax "a[i]" is not compatible with VCD format *)

let rec pp_type_expr_desc fmt te = 
  let open Format in
  let pp_sz fmt sz = match sz with
    | None -> pp_print_string fmt ""
    | Some s -> fprintf fmt "[%d]" s in
  match te with
  | TeConstr (c,[],sz) -> fprintf fmt "%s%a" c pp_sz sz
  | TeConstr (c,[t'],sz) -> fprintf fmt " %a %s%a" pp_type_expr t' c pp_sz sz
  | TeConstr (c,ts,sz) -> fprintf fmt " %a %s%a" (Rfsm.Misc.pp_list_h pp_type_expr) ts c pp_sz sz
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

