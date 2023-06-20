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
  | TeConstr of string * type_expr list * type_index_expr list (* name, args, size annotations *) 

and type_index_expr_desc =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index_expr * type_index_expr
and type_index_expr = (type_index_expr_desc,unit) Annot.t

let rec pp_type_expr_desc fmt te = 
  let open Format in
  match te with
  | TeConstr (c,[],sz) -> fprintf fmt "%s%a" c (pp_siz c) sz
  | TeConstr (c,[t'],sz) -> fprintf fmt "%a %s%a" pp_type_expr t' c (pp_siz c) sz
  | TeConstr (c,ts,sz) -> fprintf fmt "(%a) %s%a" (Rfsm.Misc.pp_list_h ~sep:"," pp_type_expr) ts c (pp_siz c) sz
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

and pp_siz c fmt sz =
  match sz with
  | [] -> ()
  | _ -> Format.fprintf fmt "<%a>" (Rfsm.Misc.pp_list_h ~sep:"," pp_type_index_expr) sz

and pp_type_index_expr_desc fmt ie = 
  let open Format in
  match ie with
  | TiConst i -> fprintf fmt "%d" i
  | TiVar v -> fprintf fmt "%s" v
  | TiBinop (op,e1,e2) -> fprintf fmt "%a%s%a" pp_type_index_expr e1 op pp_type_index_expr e2 (* TODO : add parens *)
and pp_type_index_expr fmt ie = 
  Format.fprintf fmt "%a" pp_type_index_expr_desc ie.Annot.desc 

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of string
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EBinop of string * expr * expr
  | ECon0 of string             (** Enum value *)
  | EArr of string * expr       (** t[i] when t is an array *)
  | EArrExt of expr list        
  | ECond of expr * expr * expr  (** e1 ? e2 : e3 *)

let rec pp_expr_desc fmt e = 
  let open Format in
  match e with
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EBinop (op,e1,e2) -> fprintf fmt "%a%s%a" pp_expr e1 op pp_expr e2
  | ECon0 c -> fprintf fmt "%s" c
  | EArr (a,i) -> fprintf fmt "%s[%a]" a pp_expr i
  | EArrExt vs -> fprintf fmt "[%a]" (Rfsm.Misc.pp_list_h ~sep:"," pp_expr) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" pp_expr e1 pp_expr e2 pp_expr e3
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc;

(** Assignations LHS *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of string
  | LhsArrInd of string * expr (* a[i] *)

let rec pp_lhs_desc fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%s" v
  | LhsArrInd (a,i) -> Format.fprintf fmt "%s[%a]" a pp_expr i
and pp_lhs fmt l = pp_lhs_desc fmt l.Annot.desc

let mk_simple_lhs v = Annot.make (LhsVar v)
let lhs_name l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsArrInd (a,_) -> a 

(** Inspectors *)
              
let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | _ -> []

let vars_of_lhs l = match l.Annot.desc with
  | LhsVar v -> [v]
  | LhsArrInd (a,i) -> a :: vars_of_expr i 

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
  | LhsArrInd (a,i) -> { l with Annot.desc = LhsArrInd (subst_var phi a, subst_expr phi i) } 

(** VCD interface *)
              
let vcd_name lhs =
  match lhs.Annot.desc with
  | LhsVar v -> v
  | LhsArrInd (a,i) -> a ^ "." ^ Rfsm.Misc.to_string pp_expr i (* Note: syntax "a[i]" is not compatible with VCD format *)

(** Pre-processing *)

let is_bool_type (t: type_expr) =
  match t.Annot.desc with
  | TeConstr ("bool", [], _) -> true
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
