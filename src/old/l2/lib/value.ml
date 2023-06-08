type value =
  | Val_int of int
  | Val_bool of bool
  | Val_array of value array
  | Val_enum of string
  | Val_unknown

type typ = Types.typ

let rec default_value ty = match ty with
  | Types.TyConstr("array", [t'], Some sz) -> Val_array (Array.make sz (default_value t'))
  (* The initial value of an array is _not "undefined" but an _array_ of undefined value *)
  | _ -> Val_unknown

let rec pp_value fmt v = 
  let open Format in
  match v with
  | Val_int v -> fprintf fmt "%d" v
  | Val_bool v -> fprintf fmt "%b" v
  | Val_array vs -> fprintf fmt "[|%a|]" (Msic.Misc.pp_list_h ~sep:"," pp_value) (Array.to_list vs)
  | Val_enum c -> fprintf fmt "%s" c
  | Val_unknown -> fprintf fmt "?"
