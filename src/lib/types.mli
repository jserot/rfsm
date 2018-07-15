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
  | TyUnknown
  | TyEvent
  | TyBool
  | TyEnum of string list
  | TyInt of int_range option
  | TyFloat
  | TyArray of Index.t * typ    (* size, subtype *)
  | TyVar of tvar               (* Internal use only *)
  | TyArrow of typ * typ        (* Internal use only *)
  | TyProduct of typ list       (* Internal use only *)

and tvar =
  { stamp: string;
    mutable value: typ value }

and 'a value =
  | Unknown
  | Known of 'a

and int_range = Index.t * Index.t (* min, max *)

type typ_scheme =
  { ts_params: tvar list;
    ts_body: typ }

val mk_type_var: unit -> tvar
val new_type_var: unit -> typ

val real_type: typ -> typ
  
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

val type_of_value : Expr.e_val -> typ
  (** [type_of_value v] returns the "best known" type for value [v].
      For an integer, this will alaways be [TyInt None].
      For an enumerated value [c], this will be the "approximation" [TyEnum [c]]. *)

val unify: typ -> typ -> unit
  
val type_instance: typ_scheme -> typ
  
(** {2 Printers} *)

val string_of_range : Index.t * Index.t -> string

val string_of_type_scheme : typ_scheme -> string

val string_of_type : typ -> string
