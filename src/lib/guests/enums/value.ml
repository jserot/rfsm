type value =
  | Val_int of int
  | Val_bool of bool
  | Val_array of value array
  | Val_enum of string
  | Val_unknown

type typ = Types.typ
         
let rec default_value ty = match ty with
  | Some (Types.TyConstr("array", [t'], Some sz)) -> Val_array (Array.make sz (default_value (Some t')))
  (* The initial value of an array is _not "undefined" but an _array_ of undefined value *)
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

let rec pp_value fmt v = 
  let open Format in
  match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_array vs -> fprintf fmt "[|%a|]" (Rfsm.Misc.pp_list_h ~sep:"," pp_value) (Array.to_list vs)
  | Val_enum c -> fprintf fmt "%s" c
  | Val_unknown -> fprintf fmt "?"
