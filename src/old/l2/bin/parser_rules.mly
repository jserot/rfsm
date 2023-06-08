decl:
  | d = type_decl { d }
  | d = var_decl { d }

type_decl:
  | TYPE id = LID EQUAL ENUM LBRACE ctors=separated_nonempty_list(COMMA,ctor) RBRACE
     { Lang.L.Syntax.Type_decl (mk ~loc:($symbolstartofs,$endofs) (TD_Enum (id,ctors))) }

ctor:
  | c = UID { c }

var_decl:
  | VAR id = LID COLON te = type_expr { Lang.L.Syntax.Var_decl (mk ~loc:($symbolstartofs,$endofs) (id,te)) }

action:
  | lhs = lhs COLEQ e = expr { Lang.L.Syntax.Action (mk ~loc:($symbolstartofs,$endofs) (Assign (lhs, e))) }

lhs:
  | id = LID { mk ~loc:($symbolstartofs,$endofs) (LhsVar id) }
  | id = LID LBRACKET idx = expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (LhsArrInd (id, idx)) }

type_expr:
  | tc = LID { mk ~loc:($symbolstartofs,$endofs) (TeConstr (tc, [], None)) }
  | te = type_expr TYARRAY LBRACKET sz = INT RBRACKET{ mk ~loc:($symbolstartofs,$endofs) (TeConstr ("array", [te], Some sz)) }

expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("+",e1,e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("-",e1,e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("*",e1,e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("/",e1,e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("<",e1,e2)) }
  | a = LID LBRACKET i=expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (EArr (a,i)) }

simple_expr:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar v) }
  | c = UID { mk ~loc:($symbolstartofs,$endofs) (ECon0 c) }
  | c = INT { mk ~loc:($symbolstartofs,$endofs) (EInt c) }
  | c = BOOL { mk ~loc:($symbolstartofs,$endofs) (EBool c) }
