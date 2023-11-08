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

type 'a t = (Ident.t * 'a) list

let apply phi id = List.assoc id phi

let pp pp_v fmt phi = Ext.List.pp_assoc (Ident.pp,pp_v) fmt phi (** For debug only *)
