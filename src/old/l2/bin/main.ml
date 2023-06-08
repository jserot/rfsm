open Lang
   
module C = (* The command-line compiler *)
  Msic.Clc.Make
    (L)
    (Lexer)
    (struct include Parser type pgm = L.Syntax.pgm end)
           
let _ = Printexc.print C.main ()
