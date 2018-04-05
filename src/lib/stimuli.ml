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

open Utils
   
type event = Types.date * Expr.e_val option  (* date, value (None for pure events) *)

type stimuli = Types.date * (Ident.t * Expr.e_val option) list  (* date, [name1,val1; ...; nameN,valN] *)
             
(* Builders *)

let mk_spor_event ts = List.map (function t -> t, None) ts

let mk_per_event per t1 t2 =
  let rec h t =
    if t <= t2
    then (t, None) :: h (t+per)
    else [] in
  h t1

let mk_val_changes chgs = List.map (function (t,v) -> t, Some v) chgs

let mk_stimuli id (t,v) = t, [Ident.Global id,v]
                                
let merge_stimuli (lss: stimuli list list) =
  let merge (l1: stimuli list) (l2: stimuli list) = 
    let rec h l1 l2 = match l1, l2 with
        [], [] -> []
      | l1, [] -> l1
      | [], l2 -> l2
      | (t1,evs1)::ss1, (t2,evs2)::ss2 ->
         if t1=t2 then (t1,evs1@evs2) :: h ss1 ss2
         else if t1<t2 then (t1,evs1) :: h ss1 l2
         else (t2,evs2) :: h l1 ss2 in
    h l1 l2 in
  match lss with
    [] -> invalid_arg "Stimuli.merge_events"
  | l::ls -> List.fold_left merge l ls

(* Printing *)

let string_of_event (t,v) = match v with
  None -> string_of_int t
| Some v -> string_of_int t ^ ":" ^ Expr.string_of_value v

let string_of_events evs = ListExt.to_string string_of_event "," evs
                         
let string_of_stimuli (t,evs) =
  let string_of_ev (id,v) = match v with
    | None -> Ident.to_string id
    | Some v' -> Ident.to_string id ^ "=" ^ Expr.string_of_value v' in
  "t=" ^ string_of_int t ^ ": " ^ ListExt.to_string string_of_ev " " evs
