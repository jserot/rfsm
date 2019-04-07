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

(** Misc *)

exception Internal_error of string
exception Not_implemented of string

val fatal_error : string -> 'a
val not_implemented : string -> 'a
val warning : string -> unit

val add_assoc: ('k * 'v) list -> 'k * 'v -> ('k * 'v) list
  (** [add_assoc (k,v) l] returns the assocation list obtained by adding pair [(k,v)] to [l] when key [k]
      does not appear in [l]. Returns [l] otherwise. *)

val collect_assoc: ('a -> ('k * 'v) list) -> 'a list -> ('k * 'v) list
  (** [collect f l] returns the association list built from applying [f] to each element of [l] and
      collecting all the [(k,v)] resulting pairs. *)
                                             
val fold_left: ('a -> 'b -> 'a) -> 'b list -> 'a -> 'a
  (** This variant of [List.fold_left] allows [fold_left]s to be chained with [|>] *)

val id: 'a -> 'a
  (** [id x] is simply x *)

val list_split_at: int -> 'a list -> 'a list * 'a list
  (** [list_split_at k [x1;...;xn]] returns [[x1;...;xk],[xk+1;...;xn]] if 1<=k<=n. Raises [Invalid_argument] otherwise *)
                
val bit_size: int -> int
  (** [bit_size n] returns the number of bits required for representing [n] *)

val copy_with_subst: (string * string) list -> in_channel -> out_channel -> unit
  (** [copy_with_subst [(s1,s1');...] ic oc] copies each line read from input channel [ic] to output channel [oc],
      substituting each occurrence of string [si] by [si']. *) 
