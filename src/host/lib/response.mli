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

type t = (* Server responses *)
  (* Request [GetVersion] *)
  | Version of string
  (* Request [CheckFragment] *)
  | NoErr
  | SyntaxErr
  | SemanticErr of string
  | TypingErr of string
  | OtherErr of string
  [@@deriving show]
      
val to_string: t -> string
