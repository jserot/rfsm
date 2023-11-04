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

(**{1 Event sequences} *)

(** An {e event sequence} is a sequence of dated event sets. Ex: [[{H,x<-1}@t=10; {H}@t=20, ...]].
    Event sequences are to describe both {e stimuli} and {e responses}. *)

module type EVSEQ = sig
  
  module Evset: Evset.T

  type t = Evset.t list
  type value

  val merge: t -> t -> t
  val (@@) : t -> t -> t  (** [@@] is the infix notation for [merge] *)

  val merge_all: t list -> t 
  
  val mk_periodic: Ident.t -> int -> int -> int -> t
  val mk_changes: Ident.t -> (int * value) list -> t
  val mk_sporadic: Ident.t -> int list -> t
    
  val pp: Format.formatter -> t -> unit

end
 
module Make (ES: Evset.T) : EVSEQ with module Evset = ES and type value = ES.Event.Value.t =
struct
  module Evset = ES

  type value = Evset.Event.Value.t
             
  type t = Evset.t list

  let rec merge st1 st2 =
    match st1, st2 with
    | [], _ -> st2
    | _, [] -> st1
    | evs1::rest1, evs2::rest2 ->
       let t1 = Evset.date evs1 in
       let t2 = Evset.date evs2 in
       if t1=t2 then (Evset.union evs1 evs2) :: merge rest1 rest2
       else if t1<t2 then evs1 :: merge rest1 st2
       else (* t1 > t2 *) evs2 :: merge st1 rest2
  
  let (@@) = merge
  
  let merge_all sts = List.fold_left merge [] sts
  
  let mk_periodic name p t1 t2 =
    let rec mk t =
      if t <= t2 then Evset.mk t [Evset.Event.Ev name] :: mk (t+p)
      else [] in
    mk t1
  
  let mk_changes name tvs =
    let open Evset.Event in
    List.map (fun (t,v) -> Evset.mk t [Upd (Syntax.mk_simple_lhs name, v)]) tvs
  
  let mk_sporadic name ts =
    List.map (fun t -> Evset.mk t [Evset.Event.Ev name]) ts
    
  let pp fmt es = 
    Misc.pp_list_v Evset.pp fmt es

end
