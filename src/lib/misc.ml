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

let id x = x

let add_assoc l (k,v) = if List.mem_assoc k l then l else (k,v)::l
                                                                   
let collect_assoc f l =  
  let add acc xs = List.fold_left add_assoc acc xs in
  List.fold_left (fun acc x -> add acc (f x)) [] l

let fold_left f l acc = List.fold_left f acc l

let list_split_at n l =
  let rec h n left l = match n, l with
      0, rest -> left, rest
    | n, [] -> invalid_arg "list_split_at"
    | n, x::xs -> h (n-1) (left@[x]) xs in
  h n [] l

let rec bit_size n = if n=0 then 0 else 1 + bit_size (n/2)

let copy_with_subst defns ic oc = 
  let rec subst mdefs s = match mdefs with
      [] -> s
    | (v,v')::ds -> subst ds (Str.global_replace (Str.regexp_string v) v' s)  in
   try
     while true do
       let line = input_line ic in
       Printf.fprintf oc "%s\n" (subst defns line)
     done
   with End_of_file ->
     ()

exception Internal_error of string
exception Not_implemented of string

let fatal_error msg = raise (Internal_error msg)
let not_implemented msg = raise (Not_implemented msg)
let warning msg = Printf.printf "** Warning: %s\n" msg
                
