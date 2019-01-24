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

module VarSet : Set.S with type elt = string

module Index : sig
  type t =
  | TiConst of int
  | TiVar of string
  | TiBinop of string * t * t
  type env = (string * int) list
  exception Illegal_op of string
  exception Illegal_type_index of t
  exception Unbound_type_index of string
  val subst: env -> t -> t 
  val vars_of: t -> VarSet.t
  val to_string: t -> string
  val to_string: t -> string
end

type typ =
  | TyEvent
  | TyBool
  | TyEnum of name * string list              (** Name, list of values *)
  | TyInt of siz                
  | TyFloat
  | TyChar
  | TyArray of Index.t * typ                  (** size, subtype *)
  | TyVar of typ var                          (** Internal use only *)
  | TyArrow of typ * typ                      (** Internal use only *)
  | TyProduct of typ list                     (** Internal use only *)
  | TyRecord of name * (string * typ) list    (** Name, fields *)

and siz =
  | SzExpr1 of Index.t                  (** For ints: bit width, for arrays: dimension *)
  | SzExpr2 of Index.t * Index.t        (** For ints: range, for arrays: dimensions *)
  | SzVar of siz var   

and name =
  | NmLit of string
  | NmVar of name var   

and 'a var =
  { stamp: string;             (* for debug only *)
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { ts_tparams: (typ var) list;
    ts_sparams: (siz var) list;
    ts_body: typ }

val make_var: unit -> 'a var
val new_type_var: unit -> typ
val new_size_var: unit -> siz
val new_name_var: unit -> name

val real_type: typ -> typ
val real_size: siz -> siz
val real_name: name -> name

val no_type: typ
val type_int: int list -> typ
  
(** {2 Exceptions} *)

exception TypeConflict of typ * typ
exception TypeCircularity of typ * typ

(** {2 Accessors} *)
                        
val is_event_type : typ -> bool
  (** [is_event_type t] is [true] iff [t=TyEvent] *)

val ivars_of : typ -> string list
  (** [ivars_of t] returns the list of index variables occuring in type [t] *)

val enums_of : typ -> (string * typ) list
  (** [tycons_of t] returns the list of enum constructors occuring in type [t], with the associated type *)
         
val size_of : typ -> int
  (** [size_of t] returns the "size" of type *)
         
val subtype_of: typ -> typ
  (** [subtype (TyArray (sz,ty')] returns the [ty'], ... *)

val is_lit_name: name -> bool
  (** [is_lit_name n] returns [true] iff [n=NmLit _] *)

(** {2 Typing} *)
  
val subst_indexes : Index.env -> typ -> typ
  (** [subst_indexes env t] substitutes all index variables listed in [env] in type [t] *)
  
val type_equal : strict:bool -> typ -> typ -> bool
  (** [type_equal b t t'] returns true iff types [t] and [t'] are "equivalent".
      If [b=true], equivalence means structural equivalence (so that, for instance, [TyInt None]
      is different from [TyInt (Some (lo,hi)].
      If [b=false], all [TyInt _] are equivalents and equivalence of enumerated types means
      inclusion, so that, for instance, [type_equal ~strict:false {On,Off} {On} = true] (but not
      the other way). *)

val unify: typ -> typ -> unit
  
val type_instance: typ_scheme -> typ
  
(** {2 Printers} *)

val string_of_type_scheme : typ_scheme -> string

val string_of_type : ?szvars:bool -> typ -> string

val string_of_name : name -> string
