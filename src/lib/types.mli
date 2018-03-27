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

(** Types *)

type date = int

type dir = IO_In | IO_Out | IO_Inout

type typ =
    TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option
  | TyVar of tvar           (* Internal use only *)
  | TyArrow of typ * typ    (* Internal use only *)
  | TyProduct of typ list   (* Internal use only *)

and tvar =
  { stamp: string;
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

and int_range = type_index * type_index (* min, max *)

and type_index =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * type_index * type_index

type typ_scheme =
  { ts_params: tvar list;
    ts_body: typ }

type tenv =   (** Typing environment *)
  { te_vars: (string * typ) list;
    te_ctors: (string * typ) list;
    te_prims: (string * typ_scheme) list; }

type ienv = (string * Expr.value) list  (** Index environment *)

(** {2 Exceptions} *)

exception Illegal_type_index of string * Expr.value 
exception Unbound_type_index of string
exception Unbound_id of string * string 
exception Typing_error of Expr.t * typ * typ 

(** {2 Accessors} *)
                        
val is_event_type : typ -> bool
  (** [is_event_type t] is [true] iff [t=TyEvent] *)

val ivars_of : typ -> string list
  (** [ivars_of t] returns the list of index variables occuring in type [t] *)

val enums_of : typ -> (string * typ) list
  (** [tycons_of t] returns the list of enum constructors occuring in type [t], with the associated type *)
         
(** {2 Typing} *)
  
val subst_indexes : ienv -> typ -> typ
  (** [subst_indexes env t] substitutes all index variables listed in [env] in type [t] *)
  
val type_equal : strict:bool -> typ -> typ -> bool
  (** [type_equal b t t'] returns true iff types [t] and [t'] are "equivalent".
      If [b=true], equivalence means structural equivalence (so that, for instance, [TyInt None]
      is different from [TyInt (Some (lo,hi)].
      If [b=false], all [TyInt _] are equivalents and equivalence of enumerated types means
      inclusion, so that, for instance, [type_equal ~strict:false {On,Off} {On} = true] (but not
      the other way). *)

val type_of_value : Expr.value -> typ
  (** [type_of_value v] returns the "best known" type for value [v].
      For an integer, this will alaways be [TyInt None].
      For an enumerated value [c], this will be the "approximation" [TyEnum [c]]. *)

val builtin_tenv : tenv
  (** The builtin typing environment *)
    
val type_expression : tenv -> Expr.t -> typ
  (** [type_expression env e] returns the type of expression [e] in environment [env], performing
      all required type checks. *)

(** {2 Printers} *)

val string_of_range : type_index * type_index -> string

val string_of_type : typ -> string

val dump_tenv : tenv -> unit
