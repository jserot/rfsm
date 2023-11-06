(* Template for defining the guest language values *)
(* The implementation defined in this file should match the signature [Guest.VALUE]  specified in ../../../host/lib/guest.ml *)

type t = (* TO BE FILLED *)

type typ = Types.typ
         
let default_value ty =  (* TO BE FILLED *)

exception Unsupported_vcd of t

let vcd_type v = match v with
  (* TO BE FILLED *)
  | _ -> raise (Unsupported_vcd v)

let vcd_value v = match v with
  (* TO BE FILLED *)
  | _ -> raise (Unsupported_vcd v)

let flatten ~base v = (* TO BE FILLED. Hint: result is simply [[base, v]] for unstructured values *)
                           
let pp fmt v =  (* TO BE FILLED *)
