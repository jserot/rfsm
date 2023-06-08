type value =
  | Val_int of int
  | Val_bool of bool
  | Val_enum of string
  | Val_unknown

type typ = Types.typ
         
let default_value ty = match ty with
  | _ -> Val_unknown

exception Unsupported_vcd of value
                           
let vcd_type v = match v with
  | Val_int _ -> Rfsm.Vcd_types.TyInt
  | Val_bool _ -> Rfsm.Vcd_types.TyBool
  | Val_enum _ -> Rfsm.Vcd_types.TyString
  | _ -> raise (Unsupported_vcd v)

let vcd_value v = match v with
  | Val_int v -> Rfsm.Vcd_types.Val_int v
  | Val_bool v -> Rfsm.Vcd_types.Val_bool v
  | Val_enum c -> Rfsm.Vcd_types.Val_string c
  | _ -> raise (Unsupported_vcd v)

let pp_value fmt v = 
  let open Format in
  match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_enum c -> fprintf fmt "%s" c
  | Val_unknown -> fprintf fmt "?"
