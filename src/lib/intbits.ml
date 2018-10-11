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

exception Invalid_range of int * int

let get_bits hi lo n = (n lsr lo) mod (1 lsl (hi-lo+1))

let set_bits hi lo n v =
  let v' = v mod (1 lsl (hi-lo+1)) in
  n lor (v' lsl lo)
