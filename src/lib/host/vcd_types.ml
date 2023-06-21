type vcd_typ =
  | TyInt of int option (** width in bits *)
  | TyBool
  | TyFloat
  | TyEvent
  | TyString
[@@deriving show]

type vcd_value =
  | Val_int of int
  | Val_bool of bool
  | Val_float of float
  | Val_string of string
[@@deriving show]

type vcd_signal = string * (char * vcd_typ)
[@@deriving show]

exception Unsupported
        
let start_symbol = 33 
let signal_cnt = ref start_symbol

let register_signal acc (name,ty) =
  if List.mem_assoc name acc then acc (* Already registered *)
  else
    let acc' = (name, (Char.chr !signal_cnt,ty)) :: acc in
    incr signal_cnt;
    acc'
