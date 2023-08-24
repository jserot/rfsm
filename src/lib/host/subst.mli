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

(** Substitutions *)

type 'a t = (Ident.t * 'a) list
  (** The type of substitution, subtituting identifiers by values of type ['a] *)

val apply: 'a t -> Ident.t -> 'a
  (** [apply s i] is [s(i)]. Raises [Not_found] if [i] is not bound in [s]. *)

val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit   (* For debug *)
