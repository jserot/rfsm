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

(**{1 Required signatures for the guest language} *)

(**{2 Language description }*)
module type INFO = sig
  val name: string (** Name of the guest language *)

  val version: string (** Version *)
end

(**{2 Types }*)
module type TYPES = sig 
  type typ
  (** Guest-level types *)

  (**{3 Constructors} *)

  val no_type: typ
  (** Special value denoting an undefined type *)

  val mk_type_fun: typ list -> typ -> typ
  (** [mk_type_fun [t1;...;tn] t] should return the type of a {e function} taking [n] arguments with type [t1], ..., [tn] and
      returning a result with type [t]. *)

  (**{3 Inspectors} *)

  val is_event_type: typ -> bool
  val is_bool_type: typ -> bool
  (** [is_event_type t] (resp. [is_bool_type t]) should return [true] iff type [t] is the {e event} (resp. {e boolean}) type.
      Returns [false] if the guest language has no such type. *)
    
  (**{3 Printing} *)

  val pp_typ: ?abbrev:bool -> Format.formatter -> typ -> unit
  (** [pp_type abbrev fmt t] should print a readable representation of type [t] on formatter [fmt]. The optional [abbrev] argument
      can be used to require an abbreviated form (for example, for a record, to print only the name of the record, without
      the description of the fields. *)

end
                 
(**{2 Syntax} *)
module type SYNTAX = sig 

  module Types : TYPES

  (**{3 Type expressions} *)

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

  (**{3 Type declarations} *)

  type type_decl = (type_decl_desc,Types.typ) Annot.t
  and type_decl_desc 
  (** The type of guest-level {e type declarations} (ex: type aliases, records, ...) *)

  val mk_alias_type_decl: Ident.t -> type_expr -> type_decl
  (** [mk_alias_type_decl name te] should return an {e alias} type declaration, {i i.e.} the type declaration
      making [name] a synonym for [te]. *)

  val pp_type_decl: Format.formatter -> type_decl -> unit
  (** [pp_type_decl fmt td] prints type declaration [td] on formatter [fmt]. *)

  (**{3 Expressions} *)

  type expr = (expr_desc,Types.typ) Annot.t
  and expr_desc
  (** The type of guest-level {e expressions} *)

  val vars_of_expr: expr -> Ident.t list
  (** [vars_of_expr e] should return the list of variables occuring in expression [e]. *)

  val pp_expr: Format.formatter -> expr -> unit
  (** [pp_expr fmt e] prints expression [e] on formatter [fmt]. *)

  (**{3 L-Values} *)

  type lval_desc
  and lval = (lval_desc,Types.typ) Annot.t
  (** The type of {e l-values}, occuring at the left of the [:=] symbol in assignations. A guest language
      will typically have {e variables} as l-values but can also support more elaborated forms, such as array indices
      (ex: [a[i]:=...]) or record fields (ex: [r.f := ...]). *)

  val mk_simple_lval: Ident.t -> lval
  (** [mk_simple_lval name] should return the {e l-value} designating a simple variable with name [name]. *)

  val is_simple_lval: lval -> bool
  (** [is_simple_lval l] should return [true] iff {e l-value} [l] is a simple variable *)
    
  val lval_base_name: lval -> Ident.t
  (** [lval_base_name l] should return the {e base name} of {e l-value} [l]. If [l] is a simple variable, this is simply its name. 
      For array or record access, this will typically the name of target array (resp. record). *)

  val lval_prefix: string -> lval -> lval
  (** [lval_prefix p l] should return the {e l-value} obtained by adding prefix ["p."] to the base name of {e l-value} [l].
      For example, if [l] is a simple variable named ["v"], [lval_prefix "foo" l] is the ["foo.v"]. This function 
      is used to generated {e name scopes} when dumping VCD traces. *)
    
  val lval_vcd_repr: lval -> Ident.t
  (** [lval_vcd_repr l] should return a representation of {e l-value} [l] to be used in a VCD trace file. 
      If [l] is a simple variable, this is simply its name. 
      Other cases will depend on the {e l-value} definition and the version of VCD format used.
      See for example the definition of [lval_vcd_repr] for the [full] guest language. *)

  val vars_of_lval: lval -> Ident.t list
  (** [vars_of_lval l] should return the list of variables occuring in {e l-value} [l]. This includes simple variables
      but also the name of the target array, record, etc. *)

  val pp_lval: Format.formatter -> lval -> unit
  (** [pp_lval fmt l] prints {e l-value} [l] on formatter [fmt]. *)
    
  val pp_qual_lval: Format.formatter -> lval -> unit
  (** Same as [pp_lval] but with an indication of the {!type:Ident.scope} of the {e l-value} *)
    
  (**{3 Substitutions} *)

  val subst_expr: Ident.t Subst.t -> expr -> expr
    (** [subst_id phi e] applies substitution [phi] to expression [e], substituting each occurrence of identifier
        [id] by identifier [phi id] *)

  val subst_lval: Ident.t Subst.t -> lval -> lval
    (** [subst_lval phi l] applies substitution [phi] to {e l-value} [l], substituting each occurrence of identifier
        [id] by identifier [phi id] *)

  val subst_param_type_expr: expr Subst.t -> type_expr -> type_expr
  (** [subst_type_expr phi te] applies substitution [phi] to type_expression [te], replacing all occurences of
      {e parameter} name [id] in type expression [te] by [phi id]. *)

  val subst_param_expr: expr Subst.t -> expr -> expr
    (** [subst_expr phi e] applies substitution [phi] to expression [e], substituting each occurrence of {e parameter}
        name [id] by expression [phi id] *)


  (**{3 Pre-processing} *)
    
  type ppr_env = type_expr Env.t
  (** Since pre-processing takes place at the syntax level, before typing, type information has to be
      provided explicitely by mapping identifiers to type expressions. *)

  val ppr_expr: ppr_env -> ?expected_type:type_expr option -> expr -> expr
  (** [ppr_expr env e] should return the result of pre-processing expression [e] in env [env].
      The optional argument [expected_type] can be used to perform type-dependent transformations.
      A typical usage is to rewrite [0] (resp [1]) as [false] (resp. [true]). See [guests/core/lib/syntax.ml] for example *)

  val ppr_lval: ppr_env -> lval -> lval
  (** [ppr_lval env e] should return the result of pre-processing {e l-value} [e] in env [env]. *)
  
end
  
(**{2 Typing} *)
module type TYPING = sig 

  module Syntax : SYNTAX

  module Types : TYPES

  (**{3 Typing environment} *)

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

  (**{3 Low-level interface to the the type-checking engine} *)

  val type_check: loc:Location.t -> Types.typ -> Types.typ -> unit
  (** [type_check loc t t'] should be called whenever types [t] and [t'] have to be unified at program location [loc]. *)
    
  (**{3 High-level interface to the type-checking engine} *)

  val type_type_decl: env -> Syntax.type_decl -> env
  (** [type_type_decl env td] should return the typing environment obtained by type checking the type declaration [td] in environment [env]. *)
    
  val type_expression: env -> Syntax.expr -> Types.typ
  (** [type_expression env e] should return the type of expression [e] in environment [env]. *)

  val type_of_type_expr: env -> Syntax.type_expr -> Types.typ
  (** [type_of_type_expr env te] should return the type denoted by the type expression [te] in environment [env]. *)

  val type_lval: env -> Syntax.lval -> Types.typ
  (** [type_lval env l] should return the type of {e l-value} [l] in environment [env]. *)
end
  
(**{2 Values} *)
module type VALUE = sig 

  type t
  (** The type of guest-level values *)

  type typ
  (** The type of guest-level types *)

  val default_value: typ -> t 
  (** [default_value t] should return a default value for type [t] *)
    
  (**{3 VCD interface} *)

  exception Unsupported_vcd of t

  val vcd_type: t -> Vcd_types.vcd_typ
  (** [vcd_type v] should return the VCD type corresponding to value [v].
      For example, if [v] denotes an boolean value, [vcd_type v] should return [Rfsm.Vcd_types.TyBool].
      This function should raise {!exception:Unsupported_vcd} in case of failure (if there's no corresponding VCD type) *)

  val vcd_value: t -> Vcd_types.vcd_value
  (** [vcd_value v] should return the VCD encoding of value [v].
      For example, if [v] denotes the boolean value [b], [vcd_value v] should return [Rfsm.Vcd_types.Val_bool b].
      This function should raise {!exception:Unsupported_vcd} in case of failure (if there's no corresponding VCD value) *)

  val flatten: base:Ident.t -> t -> (Ident.t * t) list
  (** [flatten base:b v] should decompose a structured value [v] into a list of qualified {e scalar} values for VCD dumping.
      For example, if [v] is a record [{x=1;y=2}], then [flatten ~base:"a" v] should be [[("a.x",1);("a.y",2)]].
      If [v] is a scalar value, then [flatten ~base:"a" v] is just [["a",v]] *)

  (**{3 Printing} *)

  val pp: Format.formatter -> t -> unit
  (** [pp fmt v] prints value [v] on formatter [fmt]. *)

end

(**{2 Static program representation} *)
module type STATIC = sig

  type expr
  (** The type of guest-level expressions *)

  type value
  (** The type of guest-level values *)

  exception Non_static_value of expr

  (**{3 Static evaluators} *)

  val eval_fn: string list -> expr -> value
  (** [eval_fn args body] should return the value representing a function taking a list of arguments [args] and returning the value
      denoted by expression [body]. For guest languages not supporting functions (such as the [core] one, for example), the return
      value is undefined. *)

  val eval: expr -> value
      (** [eval e] should return the value corresponding to the {e static} evaluation of expression [e]. This should function
      should raise {!exception:Non_static_value} is [e] cannot be evaluated statically (typically, if is not a litteral constant). *)

end

(**{2 Dynamic semantics} *)
module type EVAL = sig 

  module Syntax : SYNTAX

  module Value : VALUE

  type env = Value.t Env.t
  (** The type of dynamic environments, mapping identifiers to values. *)

  val mk_env: unit -> env
  (** [mk_env ()] should return a new, empty dynamic environment *)
    
  val upd_env: Syntax.lval -> Value.t -> env -> env
  (** [upd_env l v env] should return the environment obtained by adding the binding [(l,v)] to [env].
      If [l] is already bound in [env], the associated value is updated. *)

  (**{3 Evaluators} *)

  exception Illegal_expr of Syntax.expr
                          
  val eval_expr: env -> Syntax.expr -> Value.t
  (** [eval_expr env e] should return the value obtained by evaluating expression [e] in environment [env].
      Should raise {!exception:Illegal_expr} in case of failure. *)
    
  val eval_bool: env -> Syntax.expr -> bool
  (** [eval_bool env e] should return the boolean value obtained by evaluating expression [e] in environment [env].
      Should raise {!exception:Illegal_expr} if [e] does not denote a boolean value or in case of failure. *)

  (**{3 Printing} *)

  val pp_env: Format.formatter -> env -> unit
  (** [pp_env fmt env] should print a readable representation of environment [env] on formatter [fmt]. *)

end

(**{2 CTask backend} *)
module type CTASK = sig

  module Syntax: SYNTAX

  val pp_typed_symbol: Format.formatter -> Ident.t * Syntax.type_expr -> unit
  (** [pp_typed_symbol fmt (id,ty)] should print, on formatter [fmt], the C declaration of symbol [id] with type [ty].
      For example, for an [int] named ["x"], this should be ["int x"], and for an array of 8 [int]s named ["a"],
      this should be ["int a[8]"]. *)

  val pp_type_expr: Format.formatter -> Syntax.type_expr -> unit
  (** [pp_type_expr fmt te] should print, on formatter [fmt], the C type corresponding to type expression [te]. *)

  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  (** [pp_type_decl fmt td] should print, on formatter [fmt], the C statement corresponding to type declaration [td]. *)

  val pp_expr: Format.formatter -> Syntax.expr -> unit
  (** [pp_expr fmt e] should print, on formatter [fmt], the C expression corresponding to expression [e]. *)

end
                  
(**{2 SystemC backend} *)
module type SYSTEMC = sig

  module Static: STATIC

  module Syntax: SYNTAX

  type value

  val pp_typed_symbol: Format.formatter -> Ident.t * Syntax.type_expr -> unit
  (** [pp_typed_symbol fmt (id,ty)] should print, on formatter [fmt], the SystemC declaration of symbol [id] with type [ty]. *)

  val pp_type_expr: Format.formatter -> Syntax.type_expr -> unit
  (** [pp_type_expr fmt te] should print, on formatter [fmt], the SystemC type corresponding to type expression [te]. *)

  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  val pp_type_impl: Format.formatter -> Syntax.type_decl -> unit
  (** [pp_type_decl fmt td] (resp. [pp_type_impl fmt td]) should print, on formatter [fmt], the SystemC declaration and
      implementation (in the [.h] and [.cpp] file resp.) corresponding to type declaration [td]. *)

  val pp_expr: Format.formatter -> Syntax.expr -> unit
  (** [pp_expr fmt e] should print, on formatter [fmt], the SystemC expression corresponding to expression [e]. *)

  val pp_typ: Format.formatter -> Syntax.Types.typ -> unit
  (** [pp_typ fmt ty] should print, on formatter [fmt], the SystemC type corresponding to type [ty]. *)

  val pp_lval: Format.formatter -> Syntax.lval -> unit
  (** [pp_lval fmt l] should print, on formatter [fmt], the SystemC representation of {e l-value} [l]. *)

  val pp_value: Format.formatter -> value -> unit
  (** [pp_value fmt v] should print, on formatter [fmt], the SystemC value corresponding to value [v]. *)

end

(**{2 VHDL backend} *)
module type VHDL = sig

  module Syntax: SYNTAX

  module Static: STATIC

  type value

  val vhdl_type_of: Syntax.Types.typ -> Vhdl_types.t
  (** [vhdl_type_of ty] should return the VHDL type corresponding to type [ty]. *)

  val allowed_shared_type: Syntax.Types.typ -> bool
  (** [allowed_shared_type ty] should indicate whether type [ty] can be attributed to a VHDL shared variable. *)

  val pp_type_decl: Format.formatter -> Syntax.type_decl -> unit
  (** [pp_type_decl fmt td] should print, on formatter [fmt], the VHDL translation of type declaration [td]. *)

  val pp_expr: Format.formatter -> Syntax.expr -> unit
  (** [pp_expr fmt e] should print, on formatter [fmt], the VHDL expression corresponding to expression [e]. *)

  val pp_lval: Format.formatter -> Syntax.lval -> unit
  (** [pp_lval fmt l] should print, on formatter [fmt], the VHDL representation of {e l-value} [l]. *)

  val pp_value: Format.formatter -> value * Vhdl_types.t -> unit
  (** [pp_value fmt (v,t)] should print, on formatter [fmt], the VHDL value corresponding to value [v], with type [t]. *)

  val pp_type_fns_intf: Format.formatter -> Syntax.type_decl -> unit
  val pp_type_fns_impl: Format.formatter -> Syntax.type_decl -> unit
  (** [pp_type_fns_intf td] (resp. [pp_type_fns_impl fmt td]) should print, on formatter [fmt], the VHDL declaration and
      implementation (in the [entity] and [architecture] definition resp.) corresponding to type declaration [td]. *)

end
                  
(**{2 Error handling} *)
module type ERROR = sig

  val handle: exn -> unit
    (** This function will be called to handle guest-level exceptions, {e i.e.} exceptions raised by guest specific
    functions and not handled by the host language. *)

end

(**{2 Compiler options} *)
module type OPTIONS = sig

   val specs: (string * Arg.spec * string) list
     (** [specs] should list all guest-specific compiler options.
      These options will be added to those related to the host language. *)

end

(**{1 language global signature} *)

(** Each guest language must provide a set of modules conforming to module signature {!module-type:T}. *)

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
