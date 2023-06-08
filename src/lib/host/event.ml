module type T = sig
  module Syntax: Guest.SYNTAX
  module Value: Guest.VALUE

  type t =
    | Ev of string (* pure event *)
    | Upd of Syntax.lhs * Value.value   (* lhs <- v *)
    | StateMove of string * string     (* name, value *)
  val is_pure_event: t -> bool
  val mk_simple_upd: string -> Value.value -> t
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
  val vcd_register: Syntax.lhs -> Value.value -> Vcd_types.vcd_signal list -> Vcd_types.vcd_signal list
end

module Make
         (GS: Guest.SYNTAX)
         (GV: Guest.VALUE) (* with type typ = GS.Types.typ)*)
       : T with module Syntax = GS
            and module Value = GV =
struct
  module Syntax = GS
  module Value = GV
             
  type t =
    | Ev of string
    | Upd of Syntax.lhs * Value.value   
    | StateMove of string * string 

  let is_pure_event = function Ev _ -> true | _ -> false

  let mk_simple_upd name v = Upd (Syntax.mk_simple_lhs name, v)

  let compare = Stdlib.compare
              
  let vcd_register lhs v acc = acc

  let pp fmt s =
    let open Format in
    match s with
    | Ev e -> fprintf fmt "%s" e
    | Upd (lhs,v) -> fprintf fmt "%a<-%a" (Syntax.pp_lhs ~with_type:false) lhs Value.pp_value v
    | StateMove (f,q) -> fprintf fmt "%s<-%s" f q
end
