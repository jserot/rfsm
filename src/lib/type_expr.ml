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

(* Type expressions *)

type t = {
    te_desc: te_desc;
    mutable te_typ: Types.typ;
  }

and te_desc = 
  | TEBool
  | TEInt of int_annot
  | TEFloat
  | TEChar
  | TEEvent
  | TEName of string
  | TEArray of type_index_expr * t  (* size, type of elements *)

and int_annot =
  TA_none
| TA_size of type_index_expr
| TA_range of type_index_expr * type_index_expr (* min, max *)

and type_index_expr =
  | TEConst of int
  | TEVar of string
  | TEBinop of string * type_index_expr * type_index_expr

let rec string_of_type_index = function
    TEConst c -> string_of_int c
  | TEVar v -> v
  | TEBinop (op,e1,e2) -> string_of_type_index e1 ^ op ^ string_of_type_index e2 (* TO FIX *)
            
let string_of_int_annot = function
    TA_none -> ""
  | TA_size sz -> "<" ^ string_of_type_index sz ^ ">"
  | TA_range (lo,hi) -> "<" ^ string_of_type_index lo ^ ":" ^ string_of_type_index hi ^ ">"

let rec string_of_type_expr t = match t.te_desc with 
  | TEBool -> "bool"
  | TEInt a -> "int" ^ string_of_int_annot a
  | TEFloat -> "float"
  | TEChar -> "char"
  | TEEvent -> "event"
  | TEName n -> n
  | TEArray (sz,t') -> string_of_type_expr t' ^ " array[" ^ string_of_type_index sz ^ "]"
          
