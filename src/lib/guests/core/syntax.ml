module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
             
let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.make ~loc:(mk_location l) x

(** Type declarations *)
                
type type_decl_desc =
  | TD_Enum of string * string list (* Name, constructors *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

let rec pp_type_decl_desc fmt td = 
  let open Format in
  match td with
  | TD_Enum (name,ctors) -> fprintf fmt "(\"%s\", enum { %a })" name (Rfsm.Misc.pp_list_h ~sep:"," pp_print_string) ctors
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(** Type expressions *)
                
type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc = 
  | TeConstr of string (* name, no args here *)

let rec pp_type_expr_desc fmt te = 
  let open Format in
  match te with
  | TeConstr c -> fprintf fmt "%s" c
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of string
  | EInt of int
  | EBool of bool
  | EBinop of string * expr * expr
  | ECon0 of string (* Nullary value constructor *)

let rec pp_expr_desc ~with_type fmt e = 
  let open Format in
  match e with
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EBinop (op,e1,e2) -> fprintf fmt "%a%s%a" (pp_expr ~with_type) e1 op (pp_expr ~with_type) e2
  | ECon0 c -> fprintf fmt "%s" c
and pp_expr ?(with_type=false) fmt e =
  pp_expr_desc ~with_type fmt e.Annot.desc;
  if with_type then Format.fprintf fmt "{:%a}" (Rfsm.Misc.pp_opt Types.pp_typ) e.Annot.typ

(** Assignations LHS *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of string

let rec pp_lhs_desc ?(with_type=false) fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%s" v
and pp_lhs ?(with_type=false) fmt l =
  pp_lhs_desc ~with_type fmt l.Annot.desc;
  if with_type then Format.fprintf fmt "{:%a}" (Rfsm.Misc.pp_opt Types.pp_typ) l.Annot.typ

let mk_simple_lhs v = Annot.make (LhsVar v)
let lhs_name l = match l.Annot.desc with
  | LhsVar v -> v

(** Inspectors *)
              
let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | _ -> []

let vars_of_lhs l = match l.Annot.desc with
  | LhsVar v -> [v]

(** Substitution *)
              
let subst_var phi v = Rfsm.Misc.subst_id phi v
                    
let rec subst_expr phi e =
  match e.Annot.desc with
  | EVar v -> { e with Annot.desc = EVar (subst_var phi v) }
  | EBinop (op,e1,e2) -> { e with Annot.desc = EBinop (op, subst_expr phi e1, subst_expr phi e2) }
  | _ -> e

let subst_lhs phi l = 
  match l.Annot.desc with
  | LhsVar v -> { l with Annot.desc = LhsVar (subst_var phi v) }

(** VCD interface *)
              
let vcd_name lhs =
  match lhs.Annot.desc with
  | LhsVar v -> v

(** Pre-processing *)

let is_bool_type (t: type_expr) =
  match t.Annot.desc with
  | TeConstr "bool" -> true
  | _ -> false

let mk_bool_expr te e = match e.Annot.desc with
  | EInt 0 when is_bool_type te -> { e with Annot.desc = EBool false }
  | EInt 1 when is_bool_type te -> { e with Annot.desc = EBool true }
  | _ -> e 

let ppr_expr (env: (string * type_expr) list) e =
  (* Replace all bool expr [e op 0/1], where [e:bool] and [op] is [=] or [!=] by [e op false/true] *)
  let type_of v =
    (* Since pre-processing is carried out _before_ typing, the only type-related available information
       is given by the type expressions assigned to identifiers in the enclosing model *)
    try List.assoc v env
    with Not_found -> Rfsm.Misc.fatal_error "Core.Syntax.ppr_expr" in
  let has_bool_type v = is_bool_type (type_of v) in
  match e.Annot.desc with
  | EBinop (op, ({ Annot.desc = EVar v; _ } as e'), e'') when List.mem op ["="; "!="] && has_bool_type v  ->  
       { e with Annot.desc = EBinop (op, e', mk_bool_expr (type_of v) e'') }
  | _ -> e

let ppr_lhs _ l = l 
