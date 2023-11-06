type_decl:
 | TYPE /* Not used. There's no type declaration in the Core guest language */ { mk ~loc:$sloc () }

type_expr:
  | tc = LID { mk ~loc:$sloc (TeConstr tc) }

expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:$sloc  (mk_binop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:$sloc  (mk_binop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:$sloc  (mk_binop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:$sloc  (mk_binop ("/", e1, e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:$sloc (mk_binop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:$sloc (mk_binop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:$sloc (mk_binop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:$sloc (mk_binop ("<",e1,e2)) }

simple_expr:
  | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) }
  | e = scalar_const { e }
  | LPAREN e = expr RPAREN { e }

lhs:
  | v = LID { mk ~loc:$sloc (LhsVar (mk_ident v)) }

param_value:
  | v = scalar_const { v }

scalar_const:
  | c = INT { mk ~loc:$sloc (EInt c) }
  | MINUS c = INT { mk ~loc:$sloc (EInt (-c)) }
  | c = BOOL { mk ~loc:$sloc (EBool c) }

const:
  | c = scalar_const { c }

stim_const: 
  | c = scalar_const { c }

