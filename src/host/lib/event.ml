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

(**{1 Events} *)

module type T = sig
  module Syntax: Guest.SYNTAX
  module Value: Guest.VALUE

  type t =
    | Ev of Ident.t                 (** pure event *)
    | Upd of Syntax.lhs * Value.t   (** assignation ([lhs <- v]) *)
    | StateMove of string * string  (** state move ([src],[dst]) *)
  val is_pure_event: t -> bool
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end

module Make
         (GS: Guest.SYNTAX)
         (GV: Guest.VALUE)
       : T with module Syntax = GS
            and module Value = GV =
struct
  module Syntax = GS
  module Value = GV
             
  type t =
    | Ev of Ident.t
    | Upd of Syntax.lhs * Value.t   
    | StateMove of string * string 

  let is_pure_event = function Ev _ -> true | _ -> false

  let compare = Stdlib.compare
              
  (* let vcd_register lhs v acc = acc *)

  let pp fmt s =
    let open Format in
    match s with
    | Ev e -> fprintf fmt "%a" Ident.pp e
    | Upd (lhs,v) -> fprintf fmt "%a<-%a" Syntax.pp_lhs lhs Value.pp v
    | StateMove (f,q) -> fprintf fmt "%s<-%s" f q
end
