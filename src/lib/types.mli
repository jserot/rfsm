(** Types *)

type date = int

type dir = IO_In | IO_Out | IO_Inout

type typ =
    TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option
  | TyVar of tvar           (* Only used internally for type checking *)
  | TyArrow of typ * typ    (* Only used internally for type checking *)
  | TyProduct of typ list   (* Only used internally for type checking *)

and tvar =
  { stamp: string;             (* for debug only *)
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

and int_range = type_index * type_index (* min, max *)

and type_index =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index * type_index

exception Illegal_type_index of string * Expr.value 
exception Unbound_type_index of string

exception Unbound_id of string * string 
exception Typing_error of Expr.t * typ * typ 

val is_event_type : typ -> bool

val vars_of : typ -> string list
val tycons_of : typ -> (string * typ) list

type ienv = (string * Expr.value) list
         
val subst_indexes : ienv -> typ -> typ
  
val type_equal : strict:bool -> typ -> typ -> bool

val type_of_value : Expr.value -> typ

type typ_scheme =
  { ts_params: tvar list;
    ts_body: typ }

type tenv =
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

val builtin_tenv : tenv
    
val type_expression : tenv -> Expr.t -> typ

(** {2 Printers} *)

val string_of_range : type_index * type_index -> string

val string_of_type : typ -> string

val dump_tenv : tenv -> unit
