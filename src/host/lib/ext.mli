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

(** {1 Extensions to [Stdlib] operations and modules} *)

module Base : sig

    val pow2 : int -> int
    (** [pow2 n] is {m 2^n}. Not tail recursive. Raises [Invalid_argument] if {m n<0} *)

    val neg : ('a -> bool) -> 'a -> bool
    (** [neg f x] is [not (f x)] *)

    val swap : 'a * 'b -> 'b * 'a
    (** [swap (x,y)] is [(y,x)] *)
      
    val clone : 't -> 't
    (** [clone v] returns a deep copy of value [v] *)

  end

module Format :
  sig
    val pp_spc : Format.formatter -> unit -> unit
    val pp_cut : Format.formatter -> unit -> unit
    val to_string : (Format.formatter -> 'a -> unit) -> 'a -> string
  end

module List :
  sig
    val cart_prod : 'a list -> 'b list -> ('a * 'b) list
     (** [cart_prod l1 l2] returns the cartesian product of lists [l1] and [l2]. For instance, if [l1=[1;2]] and [l2=["a";"b"]],
         then [cart_prod l1 l2] is [[(1,"a");(1,"b");(2,"a");(2,"b")]] *)

    val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    (** [replace_assoc k v l] replaces the value bound to key [k] in association list [l] by [v].
        If [k] was bound in [l], the binding [(k,v)] is added to [l]. *)

    val add_list_assoc : 'a -> 'b -> ('a * 'b list) list -> ('a * 'b list) list
    (** [add_list_assoc k v] add value [v] to the _list of values_ bound to [k] in [l]. 
        If key [k] is not bound in [l], add it, with associated value [[v]]. *)

    val scatter : ('a -> 'b) -> 'a list -> ('b * 'a list) list
    (** [scatter f l] applies [f] to each element of [l] and returns the list of distinct results, with each result
        paired with the list of corresponding elements. 
        For example: [scatter String.length ["a";"abc";"bac";"abcdef"]] is [[(6, ["abcdef"]); (3, ["bac"; "abc"]); (1, ["a"])] ] *)

    val iter_fst : (bool -> 'a -> unit) -> 'a list -> unit
    (** [iter_fst f l] behaves like [Stdlib.List.iter f l] but the [f] function takes an extra argument indicating
        whether the argument is the first of the source list or not *)
      
    val fold_leftr : ('a -> 'b -> 'a) -> 'b list -> 'a -> 'a
    (** [fold_leftr f l z] is [Stdlib.fold_left f z l] *)
      
    val pp_v :
      (Stdlib.Format.formatter -> 'a -> unit) ->
      Stdlib.Format.formatter ->
      'a list ->
      unit
    (** [pp_v pp fmt l] prints list [l] on formatter [fmt] using printer [pp] for each element, one line per element *)

    val pp_h :
      ?sep:string ->
      (Stdlib.Format.formatter -> 'a -> unit) ->
      Stdlib.Format.formatter ->
      'a list ->
      unit
    (** [pp_h pp fmt l] prints list [l] on formatter [fmt] using printer [pp] for each element, on a single line,
        and printing the optional string [sep] between elements. *)
      
    val pp_opt :
      lr:string * string ->
      sep:string ->
      (Stdlib.Format.formatter -> 'a -> unit) ->
      Stdlib.Format.formatter ->
      'a list
      -> unit
    (** [pp_opt lr:(l,r) sep pp l] is prints [l] using [pp_h sep pp] between [l] and [r] delimiters but prints nothing  
        if [l] is empty. *)

    val pp_assoc :
      (Stdlib.Format.formatter -> 'a -> unit) * (Stdlib.Format.formatter -> 'b -> unit) ->
      Stdlib.Format.formatter ->
      ('a * 'b) list ->
      unit
    (** [pp_assoc (pp_k,pp_v) fmt l] prints association list [l] on formatter [fmt] using printer [pp_k] (resp. [pp_v]
        for printing keys (resp. values). *)

  end

module Option :
  sig
    val pp :
      ?none:string ->
      (Stdlib.Format.formatter -> 'a -> unit) ->
      Stdlib.Format.formatter ->
      'a option
      -> unit
           (** [pp ?none pp_v fmt o] prints value [v] if [o] is [Some v], using [pp_v] and prints the string
               [none] (defaulting to [""]) if [o] is [None] *)
     
  end

module File :
  sig
    val check_dir : string -> unit
    val open_file : string -> out_channel * Stdlib.Format.formatter
    val close_file : out_channel * Stdlib.Format.formatter -> unit
    val copy_with_subst :
      (string * string) list -> in_channel -> out_channel -> unit
  end
