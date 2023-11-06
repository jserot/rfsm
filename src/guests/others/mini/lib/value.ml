(* The implementation defined in this file should match the signature [Guest.VALUE]  specified in ../../../host/lib/guest.ml *)

type t =
  | Val_bool of bool
  | Val_unknown

type typ = Types.typ
         
let default_value ty = match ty with
  | _ -> Val_unknown

exception Unsupported_vcd of t

let vcd_type v = match v with
 
  | Val_bool _ -> Rfsm.Vcd_types.TyBool
  | _ -> raise (Unsupported_vcd v)

let vcd_value v = match v with
 
  | Val_bool v -> Rfsm.Vcd_types.Val_bool v
  | _ -> raise (Unsupported_vcd v)

let flatten ~base v = [base, v]  (* No structured value in this guest language *)
                           
let pp fmt v = 
  match v with
  | Val_bool v -> Format.fprintf fmt "%b" v
  | Val_unknown -> Format.fprintf fmt "???"
