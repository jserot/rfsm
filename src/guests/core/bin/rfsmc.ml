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

open Lang
   
module Compiler =
  Rfsm.Compiler.Make
    (L)
    (Lexer)
    (struct include Parser type program = L.Syntax.program end)
           
let _ = Printexc.print Compiler.main ()
