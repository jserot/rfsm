(* The implementation defined in this file should match the signature [Guest.TYPES]  specified in ../../../host/lib/guest.ml *)

type typ =
  | TyEvent
  | TyBool
  | TyUnknown

let no_type =  TyUnknown

let is_event_type t = match t with TyEvent -> true | _ -> false
let is_bool_type t =  match t with TyBool -> true | _ -> false
let mk_type_fun ty_args ty_res = TyUnknown  (* NOT USED *)

let pp_typ ~abbrev fmt t =
  let open Format in
  match t with
  | TyEvent -> fprintf fmt "event"
  | TyBool -> fprintf fmt "bool"
  | TyUnknown -> fprintf fmt "<unknown>"
