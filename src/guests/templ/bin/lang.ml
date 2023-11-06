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

(* The language itself (syntax, type-checker, evaluator) *)

(* Note: this functor application has to be put in a separate module to be referenced by the parser
   without creating a dependency cycle... *)

module L = Rfsm.Host.Make(Myguest.Top)

