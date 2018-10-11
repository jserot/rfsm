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

(** Bit level operations on [int]s *)

exception Invalid_range of int * int

val get_bits: int -> int -> int -> int
  (** [get_bits hi lo n] returns the value represented by the bit range [hi..lo] in [n]. *)

val set_bits: int -> int -> int -> int -> int
  (** [set_bits hi lo n v] returns the value obtained by setting by the bit range [hi..lo] in [n] to [v]. *)
