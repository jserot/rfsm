(* The type language for the host language. *)

(* TODO : shunt this module which is just a wrapper around  [Guest.Types] ? *)

module type TYPES = sig
  module GuestTypes: Guest.TYPES
  type typ
  val pp_typ: Format.formatter -> typ -> unit
end

module Make (BT: Guest.TYPES) =
struct
  module GuestTypes = BT
  type typ = GuestTypes.typ 
  let pp_typ = GuestTypes.pp_typ
end

