type_expr:
  | tc = LID { mk ~loc:($symbolstartofs,$endofs) (TeConstr (tc, [], None)) }
  | te = type_expr TYARRAY LBRACKET sz = INT RBRACKET{ mk ~loc:($symbolstartofs,$endofs) (TeConstr ("array", [te], Some sz)) }

expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("/", e1, e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("<",e1,e2)) }
  | a = LID LBRACKET i=expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (EArr (a,i)) }

simple_expr:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar v) }
  | e = constant { e }
  | LPAREN e = expr RPAREN { e }

lhs:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (LhsVar v) }
  | id = LID LBRACKET idx = expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (LhsArrInd (id, idx)) }

constant:
  | c = int_const { mk ~loc:($symbolstartofs,$endofs) (EInt c) }
  | c = bool_const { mk ~loc:($symbolstartofs,$endofs) (EBool c) }

int_const:
  | v = INT { v }
  | MINUS v = INT { -v }

bool_const:
  | v = BOOL { v }


