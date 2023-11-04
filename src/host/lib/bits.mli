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

(**{1 Bit-level manipulation of integers} *)

val of_int: int -> int -> string
  (** [of_int s n] returns the bit-level representation of nsigned integer [n] as a [string] of length [s] *)

val get_bits: hi:int -> lo:int -> int -> int
  (** [get_bits hi lo n] returns the integer represented by the bits ranging from [hi] to [lo] in integer [n].
      Example: [get_bits ~hi:3 ~lo:1 13] returns [6]. *)

val set_bits: hi:int -> lo:int -> dst:int -> int -> int
  (** [set_bits hi lo dst v] returns the integer obtained by setting the bits ranging from [hi] to [lo] in integer [n] to [v].
      Example: [set_bits ~hi:3 ~lo:1 ~dst:0 7] returns [14]. *)
