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

(** VCD Interface *)

(* The [Guest.Value] module must provide functions to convert types (resp. values) to [vcd_typ] and [vcd_value] *)

type vcd_typ =
  | TyInt of int option
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

val pp_vcd_signal: Format.formatter -> vcd_signal -> unit (* For debug only *)

exception Unsupported

val register_signal: vcd_signal list -> Ident.t * vcd_typ -> vcd_signal list
