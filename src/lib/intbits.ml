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
  let msk = let r = ref 0 in for i=lo to hi do r := !r lor (1 lsl i) done; !r in
  (n land (lnot msk)) lor (v' lsl lo)

let to_string s n = 
  let b = Bytes.create s in
  let rec h n i =
    if i>=0 then begin
      Bytes.set b i (if n mod 2 = 1 then '1' else '0');
      h (n/2) (i-1)
      end in
  h n (s-1);
  Bytes.to_string b

let rec bit_size n = if n=0 then 0 else 1 + bit_size (n/2)

