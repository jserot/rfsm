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

let rec bit_size n = if n=0 then 0 else 1 + bit_size (n/2)

let list_split_at n l =
  let rec h n left l = match n, l with
      0, rest -> left, rest
    | n, [] -> invalid_arg "list_split_at"
    | n, x::xs -> h (n-1) (left@[x]) xs in
  h n [] l

let left_fold f elems l = List.fold_left f l elems
      
