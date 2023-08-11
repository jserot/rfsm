open Lang
   
module C = (* The command-line compiler *)
  Rfsm.Clc.Make
    (L)
    (Lexer)
    (struct include Parser type program = L.Syntax.program end)
           
let _ = Printexc.print C.main ()
