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

type vcd_typ =
  | TyInt of int option (** width in bits *)
  | TyBool
  | TyFloat
  | TyEvent
  | TyString
  | TyChar
[@@deriving show]

type vcd_value =
  | Val_int of int
  | Val_bool of bool
  | Val_float of float
  | Val_char of char
  | Val_string of string
[@@deriving show]

type vcd_signal = Ident.t * (char * vcd_typ)
[@@deriving show]

exception Unsupported
        
let start_symbol = 33 
let signal_cnt = ref start_symbol

let register_signal acc (name,ty) =
  if List.mem_assoc name acc then acc (* Already registered *)
  else
    (* let _ = Format.printf "** Vcd_types.register_signal(%s,%a)\n" name pp_vcd_typ ty in *)
    let acc' = (name, (Char.chr !signal_cnt,ty)) :: acc in
    incr signal_cnt;
    acc'
