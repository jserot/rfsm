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

(**{1 Annotations} *)

type ('a,'b) t = {

   mutable desc: 'a; [@main] (** Annotated value *)

   mutable typ: 'b;  (** Type annotation (type ['b] will be bound to the guest language type) *)

   loc: Location.t [@default Location.no_location] (** Location in source code *)
      } 
   (** The type [('a,'b) t] is used to attach type and location annotations to values of type ['a].  *)

val mk : 
  loc:(Stdlib.Lexing.position * Stdlib.Lexing.position) ->
  typ:'b ->
  'a ->
  ('a, 'b) t
  (** [mk ~loc ~ty x] returns value [x] annotated with type [ty] and location [loc] *)

val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  (** [map f g v] returns the annotated value obtained by applying [f] and [g] to the [desc] and [typ]
      fields of [v] resp., without changing the [loc] field. *)

