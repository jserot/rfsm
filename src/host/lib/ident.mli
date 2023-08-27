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

(** Identifiers *)

type scope = Local | Global
                   
type t = {
    scope: scope;
    id: string
    }

exception Undefined of string * Location.t * t (** What, where, identifier *) 
exception Duplicate of string * Location.t * t (** What, where, identifier *) 

val mk: ?scope:scope -> string -> t

val mk_global: t -> t
  (** [mk_global i] is [{id = i.id; scope=Global}] *)
val mk_local: t -> t
  (** [mk_local i] is [{id = i.id; scope=Local}] *)

val upd_id: (string -> string) -> t -> t
  (** [upd_id f i] is [{id = f i.id; scope = i.scope}] *)

val pp: Format.formatter -> t -> unit
  (** [pp fmt i] prints identifier [i] using formatter [fmt] *)
val pp_qual: Format.formatter -> t -> unit
  (** For identifiers with a local scope, [pp_qual fmt i] is [pp fmt i].
      For identifiers with a global scope, a leading "$" is prepended to the name. *) 
  (* Note: It would be simpler to have an optional argument [?scope] to [pp]. But optional
     arguments do not work with [Format.pp] functions :( *)

val to_string: t -> string

val compare: t -> t -> int
  (** Used to define sets and maps of identifiers. Warning: [scope] is _not_ taken into account. *)
