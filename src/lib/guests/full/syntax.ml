module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
             
let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.make ~loc:(mk_location l) x

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


(** Type declarations *)
                
type type_decl_desc =
  | TD_Enum of string * string list (** Name, constructors *)
  | TD_Record of string * rfield_desc list (** Name, fields *)
  | TD_Alias of string * type_expr  (** Name, aliased type *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

and rfield_desc = string * type_expr 

let rec pp_type_decl_desc fmt td = 
  let open Format in
  let pp_rfield fmt (n,t) = fprintf fmt "%s: %a" n pp_type_expr t in
  match td with
  | TD_Enum (name,ctors) -> fprintf fmt "(\"%s\", enum { %a })" name (Rfsm.Misc.pp_list_h ~sep:"," pp_print_string) ctors
  | TD_Record (name,fields) -> fprintf fmt "(\"%s\", record { %a })" name (Rfsm.Misc.pp_list_h ~sep:";" pp_rfield) fields
  | TD_Alias (name,t) -> fprintf fmt "(\"%s\", alias %a)" name pp_type_expr t
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of string
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EChar of char
  | EBinop of string * expr * expr
  | ECon0 of string              (** Enum value *)
  | EIndexed of string * expr        (** t[i] when t is an array or an int *)
  | ERanged of string * expr * expr  (** t[hi:lo] when t is an int *)
  | EArrExt of expr list        
  | ECond of expr * expr * expr  (** e1 ? e2 : e3 *)
  | ECast of expr * type_expr
  | EFapp of string * expr list  (** f(arg1,...,argn) *)
  | ERecord of string * string   (** v.name when v is a record *)
  | ERecordExt of (string * expr) list  

let rec pp_expr_desc fmt e = 
  let open Format in
  let pp_rfield fmt (n,v) = fprintf fmt "%s=%a" n pp_expr v in
  match e with
  | EVar v -> fprintf fmt "%s" v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EChar c -> fprintf fmt "%c" c
  | EBinop (op,e1,e2) -> fprintf fmt "%a%s%a" pp_expr e1 op pp_expr e2
  | ECon0 c -> fprintf fmt "%s" c
  | EIndexed (a,i) -> fprintf fmt "%s[%a]" a pp_expr i
  | ERanged (a,hi,lo) -> fprintf fmt "%s[%a:%a]" a pp_expr lo pp_expr hi
  | EArrExt vs -> fprintf fmt "[%a]" (Rfsm.Misc.pp_list_h ~sep:"," pp_expr) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" pp_expr e1 pp_expr e2 pp_expr e3
  | ECast (e,t) -> fprintf fmt "%a::%a" pp_expr e pp_type_expr t
  | EFapp (f,es) -> fprintf fmt "%s(%a)" f (Rfsm.Misc.pp_list_h ~sep:"," pp_expr) es
  | ERecord (r,f) -> fprintf fmt "%s.%s" r f
  | ERecordExt fs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," pp_rfield) fs
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc;

(** Assignation LHS *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of string
  | LhsIndex of string * expr (* a[i] *)
  | LhsRange of string * expr * expr  (* v[hi:lo] := ... when v is an int *)
  | LhsRField of string * string             (* v.field_name when v has a record type *)

let rec pp_lhs_desc fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%s" v
  | LhsIndex (a,i) -> Format.fprintf fmt "%s[%a]" a pp_expr i
  | LhsRange (a,hi,lo) -> Format.fprintf fmt "%s[%a:%a]" a pp_expr hi pp_expr lo
  | LhsRField (r,f) -> Format.fprintf fmt "%s.%s" r f
and pp_lhs fmt l = pp_lhs_desc fmt l.Annot.desc

let mk_simple_lhs v = Annot.make (LhsVar v)

let lhs_base_name l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsIndex (a,_) -> a 
  | LhsRange (a,_,_) -> a 
  | LhsRField (a,_) -> a 

let lhs_vcd_repr l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsIndex (a,i) -> a ^ "." ^ Rfsm.Misc.to_string pp_expr i
  | LhsRange (a,hi,lo) -> a (* TO FIX ? *)
  | LhsRField (a,f) -> a ^ "." ^ f

(** Inspectors *)
              
let rec vars_of_expr e = match e.Annot.desc with
  | EVar v -> [v]
  | EInt _ | EBool _ | EFloat _ | EChar _ -> []
  | EBinop (_,e1,e2) -> vars_of_expr e1 @ vars_of_expr e2
  | ECon0 _ -> []
  | EIndexed (a,i) -> a :: vars_of_expr i
  | ERanged (v,i1,i2) -> [v] @ vars_of_expr i1 @ vars_of_expr i2
  | EArrExt vs -> List.concat (List.map vars_of_expr vs)
  | ECond (e1,e2,e3) -> vars_of_expr e1 @ vars_of_expr e2 @ vars_of_expr e3
  | ECast (e,_) -> vars_of_expr e
  | EFapp (f,es) -> List.concat (List.map vars_of_expr es)
  | ERecord (r,f) -> [r]
  | ERecordExt fs -> List.concat (List.map (fun (_,e) -> vars_of_expr e) fs)

let vars_of_lhs l = match l.Annot.desc with
  | LhsVar v -> [v]
  | LhsIndex (a,i) -> a :: vars_of_expr i 
  | LhsRange (a,hi,lo) -> [a] @ vars_of_expr hi @ vars_of_expr lo
  | LhsRField (r,f) -> [r]

(** Substitution *)
              
let subst_var phi v = Rfsm.Misc.subst_id phi v
                    
let rec subst_expr phi e =
  let subst e d = { e with Annot.desc = d } in
  match e.Annot.desc with
  | EVar v -> subst e (EVar (subst_var phi v))
  | EInt _ | EBool _ | EFloat _ | EChar _ -> e
  | EBinop (op,e1,e2) -> subst e (EBinop (op, subst_expr phi e1, subst_expr phi e2))
  | ECon0 _ -> e
  | EIndexed (a,i) -> subst e (EIndexed (subst_var phi a, subst_expr phi i))
  | ERanged (a,e1,e2) -> subst e (ERanged (subst_var phi a, subst_expr phi e1, subst_expr phi e2))
  | EArrExt vs -> subst e (EArrExt (List.map (subst_expr phi) vs))
  | ECond (e1,e2,e3) -> subst e (ECond (subst_expr phi e1, subst_expr phi e2, subst_expr phi e3))
  | ECast (e,t) -> subst e (ECast (subst_expr phi e, t))
  | EFapp (f,es) -> subst e (EFapp (f, List.map (subst_expr phi) es))
  | ERecord (r,f) -> subst e (ERecord (subst_var phi r, f))
  | ERecordExt fs -> subst e (ERecordExt (List.map (fun (n,e) -> n, subst_expr phi e) fs))

let subst_lhs phi l = 
  match l.Annot.desc with
  | LhsVar v -> { l with Annot.desc = LhsVar (subst_var phi v) }
  | LhsIndex (a,i) -> { l with Annot.desc = LhsIndex (subst_var phi a, subst_expr phi i) } 
  | LhsRField (r,f) -> { l with Annot.desc = LhsRField (subst_var phi r, f) } 
  | LhsRange (a,hi,lo) -> { l with Annot.desc = LhsRange (subst_var phi a, subst_expr phi hi, subst_expr phi lo) } 

(** VCD interface *)
              
(* let vcd_name lhs =
 *   match lhs.Annot.desc with
 *   | LhsVar v -> v
 *   | LhsIndex (a,i) -> a ^ "." ^ Rfsm.Misc.to_string pp_expr i (\* Note: syntax "a[i]" is not compatible with VCD format *\)
 *   | LhsRange (a,hi,lo) -> a ^ "." ^ Rfsm.Misc.to_string pp_expr hi ^ "." ^ Rfsm.Misc.to_string pp_expr lo
 *   | LhsRField (r,f) -> r ^ "." ^ f *)

(** Pre-processing *)

let is_bool_type (t: type_expr) =
  match t.Annot.desc with
  | TeConstr ("bool", [], _) -> true
  | _ -> false

let mk_bool_expr te e = match e.Annot.desc with
  | EInt 0 when is_bool_type te -> { e with Annot.desc = EBool false }
  | EInt 1 when is_bool_type te -> { e with Annot.desc = EBool true }
  | _ -> e 

let mkuminus name e =
  match name, e.Annot.desc with
  | "-", EInt n -> { e with Annot.desc = EInt (-n) }
  | ("-."|"-"), EFloat n -> { e with Annot.desc = EFloat (-.n) }
  | _ -> { e with Annot.desc = EFapp ("~"^name, [e]) }

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
