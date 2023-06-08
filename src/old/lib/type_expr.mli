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

val string_of_type_index: type_index_expr -> string
            
val string_of_int_annot: int_annot -> string

val string_of_type_expr: t -> string
