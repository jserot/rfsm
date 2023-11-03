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

(** Interface between the host and guest languages *)

(**{1 Guest Language description }*)

module type INFO = sig
  val name: string (** Name of the guest language *)

  val version: string (** Version *)
end

(**{1 Types }*)

module type TYPES = sig 
  type typ
  (** Guest-level types *)

  (**{2 Constructors} *)

  val no_type: typ
  (** Special value denoting an undefined type *)

  val mk_type_fun: typ list -> typ -> typ
  (** [mk_type_fun [t1;...;tn] t] should return the type of a {e function} taking [n] arguments with type [t1], ..., [tn] and
      returning a result with type [t]. *)

  (**{2 Inspectors} *)

  val is_event_type: typ -> bool
  val is_bool_type: typ -> bool
  (** [is_event_type t] (resp. [is_bool_type t]) should return [true] iff type [t] is the {e event} (resp. {e boolean}) type.
      Returns [false] if the guest language has no such type. *)
    
  (**{2 Printing} *)

  val pp_typ: abbrev:bool -> Format.formatter -> typ -> unit
  (** [pp_type abbrev fmt t] should print a readable representation of type [t] on formatter [fmt]. The [abbrev] argument
      can be used to require an abbreviated form (for example, for a record, to print only the name of the record, without
      the description of the fields. *)

end
                 
(**{1 Syntax} *)

module type SYNTAX = sig 

  module Types : TYPES

  (**{2 Type expressions} *)

  type type_expr = (type_expr_desc,Types.typ) Annot.t
  and type_expr_desc
  (** The type of guest-level {e type expressions}, denoting types *)

  val is_bool_type: type_expr -> bool
  val is_event_type: type_expr -> bool
  val is_array_type: type_expr -> bool
  (** [is_xxx_type te] should return [true] iff the type denoted by [te] is [xxx].
      Returns [false] if the guest language has no such type. *)

  val pp_type_expr: Format.formatter -> type_expr -> unit
  (** [pp_type_expr fmt te] prints type expression [te] on formatter [fmt]. *)

  (**{2 Type declarations} *)

  type type_decl = (type_decl_desc,Types.typ) Annot.t
  and type_decl_desc 
  (** The type of guest-level {e type declarations} (ex: type aliases, records, ...) *)

  val mk_alias_type_decl: Ident.t -> type_expr -> type_decl
  (** [mk_alias_type_decl name te] should return an {e alias} type declaration, {i i.e.} the type declaration
      making [name] a synonym for [te]. *)

  val pp_type_decl: Format.formatter -> type_decl -> unit
  (** [pp_type_decl fmt td] prints type declaration [td] on formatter [fmt]. *)

  (**{2 Expressions} *)

  type expr = (expr_desc,Types.typ) Annot.t
  and expr_desc
  (** The type of guest-level {e expressions} *)

  val vars_of_expr: expr -> Ident.t list
  (** [vars_of_expr e] should return the list of variables occuring in expression [e]. *)

  val pp_expr: Format.formatter -> expr -> unit
  (** [pp_expr fmt e] prints expression [e] on formatter [fmt]. *)

  (**{2 LHS} *)

  type lhs_desc
  and lhs = (lhs_desc,Types.typ) Annot.t
  (** The type of {e left-hand sides}, occuring at the left of the [:=] symbol in assignations. A guest language
      will typically have {e variables} as LHS but can also support more elaborated forms, such as array indices
      (ex: [a[i]:=...]) or record fields (ex: [r.f := ...]). *)

  val mk_simple_lhs: Ident.t -> lhs
  (** [mk_simple_lhs name] should return the LHS designating a simple variable with name [name]. *)

  val is_simple_lhs: lhs -> bool
  (** [is_simple_lhs l] should return [true] iff LHS [l] is a simple variable *)
    
  val lhs_base_name: lhs -> Ident.t
  (** [lhs_base_name l] should return the {e base name} of LHS [l]. If [l] is a simple variable, this is simply its name. 
      For array or record access, this will typically the name of target array (resp. record). *)

  val lhs_prefix: string -> lhs -> lhs
  (** [lhs_prefix p l] should return the LHS obtained by adding prefix ["p."] to the base name of LHS [l].
      For example, if [l] is a simple variable named ["v"], [lhs_prefix "foo" l] is the ["foo.v"]. This function 
      is used to generated {e name scopes} when dumping VCD traces. *)
    
  val lhs_vcd_repr: lhs -> Ident.t
  (** [lhs_vcd_repr l] should return a representation of LHS [l] to be used in a VCD trace file. 
      If [l] is a simple variable, this is simply its name. 
      Other cases will depend on the LHS definition and the version of VCD format used.
      See for example the definition of [lhs_vcd_repr] for the [full] guest language. *)

  val vars_of_lhs: lhs -> Ident.t list
  (** [vars_of_lhs l] should return the list of variables occuring in LHS [l]. This includes simple variables
      but also the name of the target array, record, etc. *)

  val pp_lhs: Format.formatter -> lhs -> unit
  (** [pp_lhs fmt l] prints LHS [l] on formatter [fmt]. *)
    
  val pp_qual_lhs: Format.formatter -> lhs -> unit
  (** Same as [pp_lhs] but with an indication of the {!type:Ident.scope} of the LHS *)
    
  (**{2 Substitutions} *)

  val subst_id: Ident.t Subst.t -> expr -> expr
    (** [subst_id phi e] applies substitution [phi] to expression [e], substituting each occurrence of identifier
        [id] by identifier [phi id] *)

  val subst_expr: expr Subst.t -> expr -> expr
    (** [subst_expr phi e] applies substitution [phi] to expression [e], substituting each occurrence of identifier
        [id] by expression [phi id] *)

  val subst_lhs: Ident.t Subst.t -> lhs -> lhs
    (** [subst_expr phi l] applies substitution [phi] to LHS [l], substituting each occurrence of identifier
        [id] by identifier [phi id] *)

  val subst_type_expr: expr Subst.t -> type_expr -> type_expr
  (** [subst_type_expr phi te] applies substitution [phi] to type_expression [te], replacing all occurences of parameter
      name [id] in type expression [te] by [phi id]. *)

  (**{2 Pre-processing} *)
    
  (** Pre-processing takes place at the syntax level, before typing. Type information is provided explicitely in
      the environment [env], mapping identifiers to type expressions. *)

  val ppr_expr: (Ident.t * type_expr) list -> expr -> expr
  (** [ppr_expr env e] should return the result of pre-processing expression [e]. *)

  val ppr_lhs: (Ident.t * type_expr) list -> lhs -> lhs
  (** [ppr_lhs env e] should return the result of pre-processing LHS [e]. *)
  
  val mk_bool_expr: type_expr -> expr -> expr
  (** [mk_bool_expr te e] should convert, when appropriate, an expression [e], with type designated by type expression [te]
      to a boolean expression. This function is typically used to convert integer values [0] and [1] to boolean values [false]
      and [true] resp. so that, for example, tests like as [e=true] or assignations like [x:=false] can be written [e=1] and
      [x:=0] resp. [mk_bool_expr] should return its argument expression [e] when there's no sensible conversion. *)

end
  
(**{1 Typing} *)

module type TYPING = sig 

  module Syntax : SYNTAX

  module Types : TYPES

  (**{2 Typing environment} *)

  type env 

  val mk_env: unit -> env
  (** [mk_env ()] should return the {e initial} typing environment. This environment should
    contain, in particular, bindings for builtins primitives, type and value constructors. *)

  val lookup_var: loc:Location.t -> Ident.t -> env -> Types.typ
  (** [lookup_var l x env] should return the type bound to identifier [x] in environment [env].
      The behaviour when [x] is not bound in [env] is left to guest definition. A possibility
      is to raise {!exception:Ident.Undefined} for example. *)
    
  val add_var: scope:Ident.scope -> env -> Ident.t * Types.typ -> env
  (** [add_var scope env (x,t)] should return the environment obtained by the binding [(x,t)] to [env]. 
      The [scope] parameter is a hint to the guest type inference system.
      Typically, it will be used to determine, in the case the added type contains type variables, whether
      these variables will be generalized ([scope=Local]) or not ([scope=Global]).
      The behaviour when [x] is already bound in [env] is left to guest definition. *)
    
  val add_param: env -> Ident.t * Syntax.expr -> env
  (** [add_param env (x,t)] should return the environment obtained by the binding [(x,t)] as a {e model parameter} to [env]. *)

  val pp_env: Format.formatter -> env -> unit
  (** [pp_env fmt env] should print a readable representation of typing environment [env] on formatter [fmt]. *)

  (**{2  Low-level interface to the the type-checking engine} *)

  val type_check: loc:Location.t -> Types.typ -> Types.typ -> unit
  (** [type_check loc t t'] should be called whenever types [t] and [t'] have to be unified at program location [loc]. *)
    
  (**{2  High-level interface to the type-checking engine} *)

  val type_type_decl: env -> Syntax.type_decl -> env
  (** [type_type_decl env td] should return the typing environment obtained by type checking the type declaration [td] in environment [env]. *)
    
  val type_expression: env -> Syntax.expr -> Types.typ
  (** [type_expression env e] should return the type of expression [e] in environment [env]. *)

  val type_of_type_expr: env -> Syntax.type_expr -> Types.typ
  (** [type_of_type_expr env te] should return the type denoted by the type expression [te] in environment [env]. *)

  val type_lhs: env -> Syntax.lhs -> Types.typ
  (** [type_lhs env l] should return the type of LHS [l] in environment [env]. *)
end
  
(**{1  Values} *)

module type VALUE = sig 
  type t
  type typ
  val default_value: typ -> t 
  (** VCD interface *)
  exception Unsupported_vcd of t
  val vcd_type: t -> Vcd_types.vcd_typ
  val vcd_value: t -> Vcd_types.vcd_value
  val flatten: base:Ident.t -> t -> (Ident.t * t) list
    (** Decomposes a structured value into a list of qualified scalar values for VCD dumping.
        For example, if [v] is a record [{x=1;y=2}], then [flatten ~base:"a" v] is [[("a.x",1);("a.y",2)]].
        If [v] is a scalar value, then [flatten ~base:"a" v] is just [["a",v]] *)
  (** Printing *)
  val pp: Format.formatter -> t -> unit
end

(**{1  Static program representation} *)

module type STATIC = sig
  type expr
  type value
  exception Non_static_value of expr
  val eval_fn: string list -> expr -> value (* Args, body *)
  val eval: expr -> value  (* Static evaluation of constant expressions *)
end

(**{1  Evaluator} *)

module type EVAL = sig 
  module Syntax : SYNTAX
  module Value : VALUE
  type env = Value.t Env.t
  val mk_env: unit -> env
  val upd_env: Syntax.lhs -> Value.t -> env -> env
  val pp_env: Format.formatter -> env -> unit
  val eval_expr: env -> Syntax.expr -> Value.t
  val eval_bool: env -> Syntax.expr -> bool
end

(**{1  CTask interface} *)

module type CTASK = sig
  module Syntax: SYNTAX
  val pp_typed_symbol: Format.formatter -> Ident.t * Syntax.type_expr -> unit
  val pp_type_expr: Format.formatter -> Syntax.type_expr -> unit
  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  val pp_expr: Format.formatter -> Syntax.expr -> unit
end
                  
(**{1  SystemC interface} *)

module type SYSTEMC = sig
  module Syntax: SYNTAX
  module Static: STATIC
  type value
  val pp_typed_symbol: Format.formatter -> Ident.t * Syntax.type_expr -> unit
  val pp_type_expr: Format.formatter -> Syntax.type_expr -> unit
  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  val pp_typ: Format.formatter -> Syntax.Types.typ -> unit
  val pp_lhs: Format.formatter -> Syntax.lhs -> unit
  val pp_expr: Format.formatter -> Syntax.expr -> unit
  val pp_value: Format.formatter -> value -> unit
  val pp_type_impl: Format.formatter -> Syntax.type_decl -> unit
end

(**{1  VHDL interface} *)

module type VHDL = sig
  module Syntax: SYNTAX
  module Static: STATIC
  type value
  val pp_type_expr: Format.formatter -> type_mark:Vhdl_types.type_mark -> Syntax.type_expr -> unit
  val pp_typ: Format.formatter -> type_mark:Vhdl_types.type_mark -> Syntax.Types.typ -> unit
  val pp_lhs: Format.formatter -> Syntax.lhs -> unit
  val pp_expr: Format.formatter -> Syntax.expr -> unit
  val pp_value: Format.formatter -> value * Vhdl_types.t -> unit
  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  val pp_type_fns_intf: Format.formatter -> Syntax.type_decl -> unit (** Auxilliary fns attached to a user-defined type *)
  val pp_type_fns_impl: Format.formatter -> Syntax.type_decl -> unit (** Auxilliary fns attached to a user-defined type *)
  val vhdl_type_of: Syntax.Types.typ -> Vhdl_types.t
  val allowed_shared_type: Syntax.Types.typ -> bool  (** Used for checking model translatability *)
end
                  
(**{1  Error handling} *)

module type ERROR = sig
  val handle: exn -> unit
end

(**{1  Guest specific error handling} *)

module type OPTIONS = sig
   val specs: (string * Arg.spec * string) list
end

(**{1  Guest language global signature} *)

(** Each guest language must provide a set of modules conforming to the following signatures *)

module type T = sig
  module Info : INFO
  module Types : TYPES
  module Syntax : SYNTAX with module Types = Types
  module Typing : TYPING with module Syntax = Syntax and module Types = Types
  module Value : VALUE with type typ = Types.typ
  module Static : STATIC with type expr = Syntax.expr and type value = Value.t
  module Eval : EVAL with module Syntax = Syntax and module Value = Value
  module Ctask: CTASK with module Syntax = Syntax
  module Systemc: SYSTEMC with module Syntax = Syntax and module Static = Static and type value = Value.t
  module Vhdl: VHDL with module Syntax = Syntax and module Static = Static and type value = Value.t
  module Error : ERROR
  module Options : OPTIONS
end
