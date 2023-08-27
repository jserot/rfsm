(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

let print_int_size = ref false (* For debug only *)

type t =
  | Val_int of int * int list (** Value, size annotation(s) *)
  | Val_bool of bool
  | Val_float of float
  | Val_char of char
  | Val_array of t array
  | Val_enum of string
  | Val_fn of string list * Syntax.expr   (** Args, body *)
  | Val_record of (string * t) list  (** (Field name, value) list *)
  | Val_unknown

type typ = Types.typ
         
let rec default_value ty = match ty with
  | Types.TyConstr ("array", [t'], Sz1 sz) ->
     (* The initial value of an array is not "undefined" but an _array_ of undefined value *)
       Val_array (Array.make sz (default_value t'))
  | Types.TyConstr ("array", _, _) ->
     Rfsm.Misc.fatal_error "Full.Value.default_value: cannot initialize unsized array"
  | Types.TyRecord (_, fs) ->
     (* The initial value of a record is not "undefined" but a _record_ of undefined value *)
     Val_record (List.map (fun (f,ty) -> f, default_value ty) fs)
  | _ -> Val_unknown

exception Unsupported_vcd of t

let flatten ~base v = 
  match v with
  | Val_record vs ->
     List.map (fun (f,v) -> Rfsm.Ident.upd_id (fun id -> id ^ "." ^ f) base, v) vs
  | _ ->
     [base, v]
                           
let vcd_type v = match v with
  | Val_int (_,[n]) -> Rfsm.Vcd_types.TyInt (Some n)
  | Val_int (_,_) -> Rfsm.Vcd_types.TyInt None
  | Val_bool _ -> Rfsm.Vcd_types.TyBool
  | Val_float _ -> Rfsm.Vcd_types.TyFloat
  | Val_char _ -> Rfsm.Vcd_types.TyChar
  | Val_enum _ -> Rfsm.Vcd_types.TyString
  | _ -> raise (Unsupported_vcd v)

let vcd_value v = match v with
  | Val_int (v,_) -> Rfsm.Vcd_types.Val_int v
  | Val_bool v -> Rfsm.Vcd_types.Val_bool v
  | Val_float v -> Rfsm.Vcd_types.Val_float v
  | Val_enum c -> Rfsm.Vcd_types.Val_string c
  | Val_char c -> Rfsm.Vcd_types.Val_char c
  | _ -> raise (Unsupported_vcd v)

let rec pp fmt v = 
  let open Format in
  let pp_rfield fmt (f,v) = fprintf fmt "%s=%a" f pp v in
  let pp_int_size fmt sz =
    if !print_int_size then fprintf fmt "<%a>" (Rfsm.Misc.pp_list_h ~sep:"," Format.pp_print_int) sz in
  match v with
  | Val_int (v,sz) -> fprintf fmt "%d%a" v pp_int_size sz
  | Val_bool v -> fprintf fmt "%b" v
  | Val_float v -> fprintf fmt "%.8f" v
  | Val_char c -> fprintf fmt "'%c'" c
  | Val_array vs -> fprintf fmt "[%a]" (Rfsm.Misc.pp_list_h ~sep:"," pp) (Array.to_list vs)
  | Val_enum c -> fprintf fmt "%s" c
  | Val_fn _ -> fprintf fmt "<fun>"
  | Val_record fs -> fprintf fmt "{%a}" (Rfsm.Misc.pp_list_h ~sep:";" pp_rfield) fs
  | Val_unknown -> fprintf fmt "?"

