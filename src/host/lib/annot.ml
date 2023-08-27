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

type ('a,'b) t = {
      mutable desc: 'a; [@main]
      mutable typ: 'b;  (* Just a placeholder here, since the associated type will be defined in the Guest language *)
      loc: Location.t [@default Location.no_location]
      } [@@deriving map]
