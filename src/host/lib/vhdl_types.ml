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

(** VHDL interface *)

type cfg = {
  mutable vhdl_bool_as_bool: bool;
  mutable vhdl_enum_prefix: string;
  mutable vhdl_use_numeric_std: bool;
  }

let cfg = {
  vhdl_bool_as_bool = false;
  vhdl_enum_prefix = "E_";
  vhdl_use_numeric_std = false;
  }

type t = 
  Std_logic 
| Unsigned of int
| Signed of int
| Integer of int_range option
| Real
| Boolean
| Char
| Array of int * t
| Enum of string * string list
| Record of string * (string * t) list
| Unknown

and int_range = int * int

type type_mark = TM_Full | TM_Abbr | TM_None

let rec pp ?(type_mark=TM_Full) fmt t =
  let open Format in
  match t, type_mark with 
    | Std_logic, _ -> fprintf fmt "std_logic"
    (* | Unsigned 1, _ -> fprintf fmt "std_logic"
     * | Signed 1, _ -> fprintf fmt "std_logic" *)
    | Unsigned _, TM_None -> fprintf fmt "unsigned"
    | Unsigned n, _ -> fprintf fmt "unsigned(%d downto 0)" (n-1)
    | Signed _, TM_None -> fprintf fmt "signed"
    | Signed n, _ -> fprintf fmt "signed(%d downto 0)" (n-1)
    | Integer (Some (lo,hi)), TM_Full -> fprintf fmt "integer range %d to %d" lo hi
    | Integer _, _ -> fprintf fmt "integer"
    | Real, _ -> fprintf fmt "real"
    | Boolean, _ -> fprintf fmt "boolean"
    | Char, _ -> fprintf fmt "character"
    | Array (n,t'), TM_Full -> fprintf fmt "array (0 to %d) of %a" (n-1) (pp ~type_mark) t'
    | Array (n,t'), _ -> fprintf fmt "array_%d_%a" n (pp ~type_mark) t'
    | Enum (n,_), _ -> fprintf fmt "%s" n
    | Record (n,_), _ -> fprintf fmt "%s" n
    | Unknown, _ -> fprintf fmt "<unknown>" 
