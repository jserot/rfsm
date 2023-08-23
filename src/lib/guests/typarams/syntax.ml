module Types = Types

module Location = Rfsm.Location
module Annot = Rfsm.Annot
             
let mk_location (p1,p2) = Location.Loc (!Location.input_name, p1, p2)
let mk ~loc:l x = Annot.{ desc=x; typ=Types.no_type; loc=mk_location l }

type ident = Rfsm.Ident.t
let pp_ident = Rfsm.Ident.pp
let mk_ident = Rfsm.Ident.mk
let mk_global_ident = Rfsm.Ident.mk ~scope:Global

(** Type expressions *)

type type_expr = (type_expr_desc,Types.typ) Annot.t
and type_expr_desc = 
  | TeConstr of ident * type_expr list * size_annot (* name, args, size annotations *) 

and size_annot = type_size list (* []: none, [s] size for ints and arrays, [lo;hi] range for ints *)

and type_size = 
  | SzParam of Rfsm.Ident.t 
  | SzConst of int

let rec pp_type_expr_desc fmt te = 
 let open Format in
  match te with
  | TeConstr (c,[],sz) -> fprintf fmt "%a%a" pp_ident c (pp_size c) sz
  | TeConstr (c,[t'],sz) -> fprintf fmt "%a %a%a" pp_type_expr t' pp_ident c (pp_size c) sz
  | TeConstr (c,ts,sz) -> fprintf fmt "(%a) %a%a" (Rfsm.Misc.pp_list_h ~sep:"," pp_type_expr) ts pp_ident c (pp_size c) sz
and pp_type_expr fmt te = 
  Format.fprintf fmt "%a" pp_type_expr_desc te.Annot.desc 

and pp_size c fmt sz =
  match sz with
  | [] -> ()
  | _ -> Format.fprintf fmt "<%a>" (Rfsm.Misc.pp_list_h ~sep:"," pp_type_size) sz

and pp_type_size fmt sz = 
  match sz with
  | SzParam i -> Format.fprintf fmt "%a" Rfsm.Ident.pp i
  | SzConst n -> Format.fprintf fmt "%d" n

(** Type declarations *)
                
type type_decl_desc =
  | TD_Enum of ident * string list (** Name, constructors *)
  | TD_Record of ident * rfield_desc list (** Name, fields *)
  | TD_Alias of ident * type_expr  (** Name, aliased type *)
and type_decl = (type_decl_desc,Types.typ) Annot.t

and rfield_desc = string * type_expr 

let mk_alias_type_decl name te = 
  Rfsm.Annot.{ desc=TD_Alias (name, te); typ=Types.no_type; loc=Location.no_location }

let rec pp_type_decl_desc fmt td = 
  let open Format in
  let pp_rfield fmt (n,t) = fprintf fmt "%s: %a" n pp_type_expr t in
  match td with
  | TD_Enum (name,ctors) -> fprintf fmt "(\"%a\", enum { %a })" pp_ident name (Rfsm.Misc.pp_list_h ~sep:"," pp_print_string) ctors
  | TD_Record (name,fields) -> fprintf fmt "(\"%a\", record { %a })" pp_ident name (Rfsm.Misc.pp_list_h ~sep:";" pp_rfield) fields
  | TD_Alias (name,t) -> fprintf fmt "(\"%a\", alias %a)" pp_ident name pp_type_expr t
and pp_type_decl fmt td = Format.fprintf fmt "%a" pp_type_decl_desc td.Annot.desc

(** Expressions *)
  
type expr = (expr_desc,Types.typ) Annot.t
and expr_desc = 
  | EVar of ident
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EChar of char
  | EBinop of ident * expr * expr
  | ECon0 of ident              (** Enum value *)
  | EIndexed of ident * expr        (** t[i] when t is an array or an int *)
  | ERanged of ident * expr * expr  (** t[hi:lo] when t is an int *)
  | EArrExt of expr list        
  | ECond of expr * expr * expr  (** e1 ? e2 : e3 *)
  | ECast of expr * type_expr
  | EFapp of ident * expr list  (** f(arg1,...,argn) *)
  | ERecord of ident * string   (** v.name when v is a record *)
  | ERecordExt of (string * expr) list  

let rec pp_expr_desc fmt e = 
  let open Format in
  let pp_rfield fmt (n,v) = fprintf fmt "%s=%a" n pp_expr v in
  match e with
  | EVar v -> fprintf fmt "%a" pp_ident v
  | EInt i -> fprintf fmt "%d" i
  | EBool b -> fprintf fmt "%b" b
  | EFloat f -> fprintf fmt "%f" f
  | EChar c -> fprintf fmt "%c" c
  | EBinop (op,e1,e2) -> fprintf fmt "%a%a%a" pp_expr e1 pp_ident op pp_expr e2
  | ECon0 c -> fprintf fmt "%a" pp_ident c
  | EIndexed (a,i) -> fprintf fmt "%a[%a]" pp_ident a pp_expr i
  | ERanged (a,hi,lo) -> fprintf fmt "%a[%a:%a]" pp_ident a pp_expr hi pp_expr lo
  | EArrExt vs -> fprintf fmt "[%a]" (Rfsm.Misc.pp_list_h ~sep:"," pp_expr) vs
  | ECond (e1,e2,e3) -> fprintf fmt "%a?%a:%a" pp_expr e1 pp_expr e2 pp_expr e3
  | ECast (e,t) -> fprintf fmt "%a::%a" pp_expr e pp_type_expr t
  | EFapp (f,es) -> fprintf fmt "%a(%a)" pp_ident f (Rfsm.Misc.pp_list_h ~sep:"," pp_expr) es
  | ERecord (r,f) -> fprintf fmt "%a.%s" pp_ident r f
  | ERecordExt fs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:"," pp_rfield) fs
and pp_expr fmt e = pp_expr_desc fmt e.Annot.desc

(** Assignation LHS *)
  
type lhs = (lhs_desc,Types.typ) Annot.t
and lhs_desc = 
  | LhsVar of ident
  | LhsIndex of ident * expr         (* a[i] *)
  | LhsRange of ident * expr * expr  (* v[hi:lo] := ... when v is an int *)
  | LhsRField of ident * string      (* v.field_name when v has a record type *)

let rec pp_lhs_desc ~pp_ident fmt l = match l with
  | LhsVar v -> Format.fprintf fmt "%a" pp_ident v
  | LhsIndex (a,i) -> Format.fprintf fmt "%a[%a]" pp_ident a pp_expr i
  | LhsRange (a,hi,lo) -> Format.fprintf fmt "%a[%a:%a]" pp_ident a pp_expr hi pp_expr lo
  | LhsRField (r,f) -> Format.fprintf fmt "%a.%s" pp_ident r f
and pp_lhs fmt l =
  pp_lhs_desc ~pp_ident:Rfsm.Ident.pp fmt l.Annot.desc
and pp_qual_lhs fmt l =
  pp_lhs_desc ~pp_ident:Rfsm.Ident.pp_qual fmt l.Annot.desc

let mk_simple_lhs v = Annot.{ desc=LhsVar v; typ=Types.no_type; loc=Location.no_location }

let is_simple_lhs l = 
  match l.Annot.desc with
  | LhsVar _ ->  true
  | _ -> false

let lhs_prefix pfx l =  (* TODO: replace this by explicit scoping of Ident.t's ? *)
  let mk d = { l with Annot.desc = d } in
  let p v = Rfsm.Ident.upd_id (fun x -> pfx ^ "." ^ x) v in
  match l.Annot.desc with
  | LhsVar v -> mk (LhsVar (p v))
  | LhsIndex (a,i) -> mk (LhsIndex (p a,i))
  | LhsRange (a,i1,i2) -> mk (LhsRange (p a,i1,i2))
  | LhsRField (a,f) -> mk (LhsRField (p a,f))

let lhs_base_name l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsIndex (a,_) -> a 
  | LhsRange (a,_,_) -> a 
  | LhsRField (a,_) -> a 

let lhs_vcd_repr l = match l.Annot.desc with
  | LhsVar v -> v
  | LhsIndex (a,i) -> Rfsm.Ident.upd_id (fun x -> x ^ "." ^ Rfsm.Misc.to_string pp_expr i) a 
  | LhsRange (a,hi,lo) -> a (* TO FIX ? *)
  | LhsRField (a,f) -> Rfsm.Ident.upd_id (fun x -> x ^ "." ^ f) a

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

(** Substitutions *)

exception Invalid_parameter of Rfsm.Location.t * Rfsm.Ident.t
              
let subst_var phi v = 
  try Rfsm.Subst.apply phi v
  with Not_found -> v
                    
let rec subst_id phi e =
  let subst e d = { e with Annot.desc = d } in
  match e.Annot.desc with
  | EVar v -> subst e (EVar (subst_var phi v))
  | EInt _ | EBool _ | EFloat _ | EChar _ -> e
  | EBinop (op,e1,e2) -> subst e (EBinop (op, subst_id phi e1, subst_id phi e2))
  | ECon0 _ -> e
  | EIndexed (a,i) -> subst e (EIndexed (subst_var phi a, subst_id phi i))
  | ERanged (a,e1,e2) -> subst e (ERanged (subst_var phi a, subst_id phi e1, subst_id phi e2))
  | EArrExt vs -> subst e (EArrExt (List.map (subst_id phi) vs))
  | ECond (e1,e2,e3) -> subst e (ECond (subst_id phi e1, subst_id phi e2, subst_id phi e3))
  | ECast (e,t) -> subst e (ECast (subst_id phi e, t))
  | EFapp (f,es) -> subst e (EFapp (f, List.map (subst_id phi) es))
  | ERecord (r,f) -> subst e (ERecord (subst_var phi r, f))
  | ERecordExt fs -> subst e (ERecordExt (List.map (fun (n,e) -> n, subst_id phi e) fs))

let subst_lhs phi l = 
  match l.Annot.desc with
  | LhsVar v -> { l with Annot.desc = LhsVar (subst_var phi v) }
  | LhsIndex (a,i) -> { l with Annot.desc = LhsIndex (subst_var phi a, subst_id phi i) } 
  | LhsRField (r,f) -> { l with Annot.desc = LhsRField (subst_var phi r, f) } 
  | LhsRange (a,hi,lo) -> { l with Annot.desc = LhsRange (subst_var phi a, subst_id phi hi, subst_id phi lo) } 

let rec subst_expr phi e =
  let subst e d = { e with Annot.desc = d } in
  match e.Annot.desc with
  | EVar v -> if List.mem_assoc v phi then List.assoc v phi else e
  | EInt _ | EBool _ | EFloat _ | EChar _ | ECon0 _ -> e
  | EBinop (op,e1,e2) -> subst e (EBinop (op, subst_expr phi e1, subst_expr phi e2))
  | EIndexed (a,i) -> subst e (EIndexed (a, subst_expr phi i))
  | ERanged (a,e1,e2) -> subst e (ERanged (a, subst_expr phi e1, subst_expr phi e2))
  | EArrExt vs -> subst e (EArrExt (List.map (subst_expr phi) vs))
  | ECond (e1,e2,e3) -> subst e (ECond (subst_expr phi e1, subst_expr phi e2, subst_expr phi e3))
  | ECast (e,t) -> subst e (ECast (subst_expr phi e, subst_type_expr phi t))
  | EFapp (f,es) -> subst e (EFapp (f, List.map (subst_expr phi) es))
  | ERecord (r,f) -> e
  | ERecordExt fs -> subst e (ERecordExt (List.map (fun (n,e) -> n, subst_expr phi e) fs))

and subst_type_expr phi te = 
  match te.Annot.desc with
  | TeConstr (c,args,szs) ->
     { te with Annot.desc = TeConstr (c,
                                      List.map (subst_type_expr phi) args,
                                      List.map (subst_size_expr ~loc:te.Annot.loc phi) szs) }

and subst_size_expr ~loc phi sz =
  match sz with
  | SzParam p ->
     begin match List.assoc_opt p phi with
     | Some { Annot.desc = EInt n; _ } -> SzConst n 
     | Some _ -> raise (Invalid_parameter (loc,p))
     | None -> raise (Rfsm.Ident.Undefined ("parameter",loc,p))
     end
  | _ -> sz
       
(** Pre-processing *)

let is_con_type c (t: type_expr) =
  match t.Annot.desc with
  | TeConstr (c', _, _) when c'.Rfsm.Ident.id = c -> true
  | _ -> false

let is_bool_type (t: type_expr) = is_con_type "bool" t
let is_int_type (t: type_expr) = is_con_type "int" t
let is_event_type (t: type_expr) = is_con_type "event" t
let is_array_type (t: type_expr) = is_con_type "array" t

let mk_bool_expr te e = match e.Annot.desc with
  | EInt 0 when is_bool_type te -> { e with Annot.desc = EBool false }
  | EInt 1 when is_bool_type te -> { e with Annot.desc = EBool true }
  | _ -> e 

let mkuminus name e =
  match name, e.Annot.desc with
  | "-", EInt n -> { e with Annot.desc = EInt (-n) }
  | ("-."|"-"), EFloat n -> { e with Annot.desc = EFloat (-.n) }
  | _ -> { e with Annot.desc = EFapp (mk_global_ident ("~"^name), [e]) }

let ppr_expr env e =
  (* Replace all bool expr [e op 0/1], where [e:bool] and [op] is [=] or [!=] by [e op false/true] *)
  let type_of v =
    (* Since pre-processing is carried out _before_ typing, the only type-related available information
       is given by the type expressions assigned to identifiers in the enclosing model *)
    try List.assoc v env
    with Not_found -> Rfsm.Misc.fatal_error "Guest.Syntax.ppr_expr" in
  let has_bool_type v = is_bool_type (type_of v) in
  match e.Annot.desc with
  | EBinop (op, ({ Annot.desc = EVar v; _ } as e'), e'') when List.mem op.Rfsm.Ident.id ["="; "!="] && has_bool_type v  ->  
       let e''' = { e with Annot.desc = EBinop (op, e', mk_bool_expr (type_of v) e'') } in
       e'''
  | _ -> e

let ppr_lhs _ l = l 

