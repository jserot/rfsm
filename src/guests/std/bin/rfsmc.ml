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
   
module C = (* The command-line compiler *)
  Rfsm.Compiler.Make
    (L)
    (Lexer)
    (struct
      include Parser
      type program = L.Syntax.program
      type fragment = L.Syntax.fragment
    end)
           
let _ = Printexc.print C.main ()
