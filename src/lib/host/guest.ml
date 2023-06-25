module type INFO = sig
  val version: string
end

module type TYPES = sig 
  type typ
  type typ_scheme
  val mk_type_constr0: string -> typ
  val is_type_constr0: string -> typ -> bool
  val mk_type_fun: typ list -> typ -> typ
  val pp_typ: Format.formatter -> typ -> unit
  val pp_typ_scheme: Format.formatter -> typ_scheme -> unit
end
                 
module type SYNTAX = sig 
  module Types : TYPES
  type type_decl_desc
  type type_decl = (type_decl_desc,Types.typ) Annot.t
  type expr_desc
  type expr = (expr_desc,Types.typ) Annot.t
  type type_expr_desc
  type type_expr = (type_expr_desc,Types.typ) Annot.t
  type lhs_desc
  type lhs = (lhs_desc,Types.typ) Annot.t
  val is_bool_type: type_expr -> bool
  val mk_bool_expr: type_expr -> expr -> expr
  val lhs_base_name: lhs -> string
  val lhs_vcd_repr: lhs -> string
  val lhs_prefix: string -> lhs -> lhs
  val mk_simple_lhs: string -> lhs
  val subst_expr: (string * string) list -> expr -> expr
  val subst_lhs: (string * string) list -> lhs -> lhs
  val vars_of_expr: expr -> string list
  val vars_of_lhs: lhs -> string list
  (** Printing *)
  val ppr_expr: (string * type_expr) list -> expr -> expr
  val ppr_lhs: (string * type_expr) list -> lhs -> lhs
  val pp_type_decl: Format.formatter -> type_decl -> unit
  val pp_type_expr: Format.formatter -> type_expr -> unit
  val pp_expr: Format.formatter -> expr -> unit
  val pp_lhs: Format.formatter -> lhs -> unit
end
  
module type TYPING = sig 
  module Syntax : SYNTAX
  module Types : TYPES
  type env
  val mk_env: unit -> env
  val lookup_var: loc:Location.t -> string -> env -> Types.typ
  val add_var: env -> string * Types.typ -> env
  (* TODO : add_prim, add_constr, ... *)
  val pp_env: Format.formatter -> env -> unit
  (* Low-level interface to the the type-checking engine *)
  val type_check: loc:Location.t -> Types.typ -> Types.typ -> unit
  (* High-level interface *)
  val type_type_decl: env -> Syntax.type_decl -> env
  val type_expression: env -> Syntax.expr -> Types.typ
  val type_of_type_expr: env -> Syntax.type_expr -> Types.typ
  val type_lhs: env -> Syntax.lhs -> Types.typ
  (* TODO : type_type_decl, type_fun_decl, ... ? *)
end
  
(* The values of the guest language must match the following signature *)

module type VALUE = sig 
  type t
  type typ
  val default_value: typ option -> t 
  (** VCD interface *)
  exception Unsupported_vcd of t
  val vcd_type: t -> Vcd_types.vcd_typ
  val vcd_value: t -> Vcd_types.vcd_value
  val flatten: base:string -> t -> (string * t) list
    (** Decomposes a structured value into a list of qualified scalar values for VCD dumping.
        For example, if [v] is a record [{x=1;y=2}], then [flatten ~base:"a" v] is [[("a.x",1);("a.y",2)]].
        If [v] is a scalar value, then [flatten ~base:"a" v] is just [["a",v]] *)
  (** Printing *)
  val pp: Format.formatter -> t -> unit
end

module type STATIC = sig
  type expr
  type value
  exception Non_static_value of expr
  val eval_fn: string list -> expr -> value (* Args, body *)
  val eval: expr -> value  (* Static evaluation of constant expressions *)
end

(* The evaluator for the guest language must match the following signature *)

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

module type ERROR = sig
  val handle: exn -> unit
end

module type OPTIONS = sig
   val specs: (string * Arg.spec * string) list
end

(* The guest language must match the following signature *)
                 
module type T = sig
  module Info : INFO
  module Types : TYPES
  module Syntax : SYNTAX with module Types = Types
  module Typing : TYPING with module Types = Types and module Syntax = Syntax
  module Value : VALUE with type typ = Types.typ
  module Static : STATIC with type expr = Syntax.expr and type value = Value.t
  module Eval : EVAL with module Syntax = Syntax and module Value = Value
  module Error : ERROR
  module Options : OPTIONS
end
