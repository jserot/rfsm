open Full2.Top.Syntax

let mk_binop (op,e1,e2) = EBinop (Rfsm.Ident.mk ~scope:Rfsm.Ident.Global op, e1, e2)
