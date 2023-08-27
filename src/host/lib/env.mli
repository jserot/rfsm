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

(** Environments *)

type 'a t
   (** The type of environments, mapping identifiers, of type [Ident.t] to values of type ['a] *)

val empty: 'a t
   (** The empty environment *)

val is_empty: 'a t -> bool
   (** [is_empty e] is [true] iff [e] is empty *)

val bindings: 'a t -> (Ident.t * 'a) list
   (** [bindings e] returns the list of bindings in [e] *)

val dom: 'a t -> Ident.t list
   (** [dom e] returns the domain of [e], i.e. the list of identifiers bounds in [e] *)

val init: (Ident.t * 'a) list -> 'a t
  (** [init] builds an environment from a list of bindings *)
  
val add : Ident.t -> 'a -> 'a t -> 'a t
  (** [add i v e] returns the environment obtained by adding binding [i->v] to [e].
      The benavior is the same as for [Stdlib.Map.S.add]: if [i] was already bound in [e] to a value that is physically equal to [v],
      [e] is returned unchanged. Otherwise, the previous binding of [i] in [e] disappears. *)

val union: 'a t -> 'a t -> 'a t 
  (** [union e1 e2] returns the union of environments [e1] and [e2].
      If an identifier is bound both in [e1] and [e2], that from [e1] disappears. *)

val mem : Ident.t -> 'a t -> bool
  (** [mem i e] returns true if [e] contains a binding for [i], false otherwise. *)

val find : Ident.t -> 'a t -> 'a
  (** [find i e] returns the value bound to [i] in [e]. Raises [Not_found] if no binding for [i] exists. *)

val upd : Ident.t -> 'a -> 'a t -> 'a t
  (** [upd i v e] returns a environment in which the value bound to [i] in [e] has been replaced by [v].
      Returns [e] unchanged if [i] is not bound in [e]. *)

val filter : (Ident.t -> 'a -> bool) -> 'a t -> 'a t
val fold : (Ident.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : (Ident.t -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
  (** [filter], [fold], [iter] and [map] are the classical higher-order functions operating on environments. *)

val pp:
     ?sep:string
  -> ?vlayout:bool
  -> ?qual:bool
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t -> unit
  (** [pp p fmt e] prints environment [e] using the formatter [fmt] and the pretty-printing function [p] for 
      printing the bound values. Optional arguments [sep], [vlayout] and [qual] are respectively used to specify
      - the binding symbol (default: "=")
      - whether bindings are printed on a single horizontal box (default) or on vertically aligned boxes
      - whether to print qualfied identifiers or not (default) *)

val pp_dom: pp_ident:(Format.formatter -> Ident.t -> unit) -> Format.formatter -> 'a t -> unit
  (** [pp_dom ~pp_ident fmt e] printfs the domain of environment [e] using the formatter [fmt] and
      the pretty-printing function [pp_ident] to print identifiers *)
