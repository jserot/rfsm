type_decl:
  | TYPE /* Not used. There's no type declaration in the Core guest language */ { mk ~loc:($symbolstartofs,$endofs) () }
/* TYPE id = LID EQUAL ENUM LBRACE ctors=separated_nonempty_list(COMMA,ctor) RBRACE */
/*      { mk ~loc:($symbolstartofs,$endofs) (TD_Enum (id,ctors)) } */

/* ctor: */
/*   | c = UID { c } */

type_expr:
  | tc = LID { mk ~loc:($symbolstartofs,$endofs) (TeConstr tc) }

expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (mk_binop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (mk_binop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (mk_binop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (mk_binop ("/", e1, e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (mk_binop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (mk_binop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (mk_binop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (mk_binop ("<",e1,e2)) }

simple_expr:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar (mk_ident v)) }
  | e = scalar_const { e }
  | LPAREN e = expr RPAREN { e }

lhs:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (LhsVar (mk_ident v)) }

param_value:
  | v = scalar_const { v }

scalar_const:
  | c = INT { mk ~loc:($symbolstartofs,$endofs) (EInt c) }
  | MINUS c = INT { mk ~loc:($symbolstartofs,$endofs) (EInt (-c)) }
  | c = BOOL { mk ~loc:($symbolstartofs,$endofs) (EBool c) }

const:
  | c = scalar_const { c }

stim_const: 
  | c = scalar_const { c }

