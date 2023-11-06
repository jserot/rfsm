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
      } [@@deriving map]
   (** The type [('a,'b) t] is used to attach type and location annotations to values of type ['a].  *)



