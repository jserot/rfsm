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

(** Global objects (IOs and shared values) *)

type global =
    GInp of string * Types.typ * stim_desc       (** Global input, with type and description of associated stimuli *)
  | GOutp of string * Types.typ                  (** Global output *)
  | GShared of string * Types.typ                (** Shared variable or event *)
  | GConst of string * Expr.value                (** Global constant *)
  | GFun of string * fn_desc                     (** Global function *)

and stim_desc = 
    Periodic of int * int * int             (** Period, start time, end time *)
  | Sporadic of int list                    (** Dates *)
  | ValueChange of (int * Expr.value) list  (** (Date,value)s *)

and fn_desc = string list * Expr.t          (** Args, body *)

let global_id = function
      GInp (id, _, _) -> id
    | GOutp (id, _) -> id
    | GShared (id, _) -> id
    | GFun (id, _) -> id
    | GConst (id, _) -> id
                       
let string_of_value_change (t,v) = string_of_int t ^ ":" ^ Expr.string_of_value v

let string_of_stim =
  let open Utils in
  function
 | Periodic (x,y,z) -> "Periodic(" ^ ListExt.to_string string_of_int ", " [x;y;z] ^ ")"
 | Sporadic ts -> "Sporadic(" ^ ListExt.to_string string_of_int ", " ts ^ ")"
 | ValueChange vs -> "ValueChange(" ^ ListExt.to_string string_of_value_change ", " vs ^ ")"
