type_decl:
  | TYPE id = LID EQUAL ENUM LBRACE ctors=separated_nonempty_list(COMMA,ctor) RBRACE
     { mk ~loc:($symbolstartofs,$endofs) (TD_Enum (id,ctors)) }

ctor:
  | c = UID { c }

type_expr:
  | TYINT sz=int_annot
    { mk ~loc:($symbolstartofs,$endofs) (TeConstr ("int", [], sz)) }
  | t=type_expr TYARRAY LBRACKET s=array_size RBRACKET
    { mk ~loc:($symbolstartofs,$endofs) (TeConstr ("array", [t], s)) }
  | c = LID
    { mk ~loc:($symbolstartofs,$endofs) (TeConstr (c, [], [])) }

int_annot:
    | (* Nothing *)
      { [] }
    | LT w=type_index_expr GT
        { [w] }
    | LT lo=type_index_expr COLON hi=type_index_expr GT
        { [lo; hi] }

array_size:
    | sz = type_index_expr { [sz] }

type_index_expr:
  | c = int_const
      { mk ~loc:($symbolstartofs,$endofs) (TiConst c) }
  | i = LID
      { mk ~loc:($symbolstartofs,$endofs) (TiVar i) }
  | LPAREN e = type_index_expr RPAREN
      { e }
  | e1 = type_index_expr PLUS e2 = type_index_expr
      { mk ~loc:($symbolstartofs,$endofs) (TiBinop ("+", e1, e2)) }
  | e1 = type_index_expr MINUS e2 = type_index_expr
      { mk ~loc:($symbolstartofs,$endofs) (TiBinop ("-", e1, e2)) }
  | e1 = type_index_expr TIMES e2 = type_index_expr
      { mk ~loc:($symbolstartofs,$endofs) (TiBinop ("*", e1, e2)) }
  | e1 = type_index_expr DIV e2 = type_index_expr
      { mk ~loc:($symbolstartofs,$endofs) (TiBinop ("/", e1, e2)) }
  | e1 = type_index_expr MOD e2 = type_index_expr
      { mk ~loc:($symbolstartofs,$endofs) (TiBinop ("mod", e1, e2)) }

expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("/", e1, e2)) }
  | e1 = expr MOD e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("%", e1, e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("<",e1,e2)) }
  | e1 = expr GTE e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop (">=",e1,e2)) }
  | e1 = expr LTE e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("<=",e1,e2)) }
  | e1 = expr LAND e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("&",e1,e2)) }
  | e1 = expr LOR e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("|",e1,e2)) }
  | e1 = expr LXOR e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("^",e1,e2)) }
  | e1 = expr SHR e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop (">>",e1,e2)) }
  | e1 = expr SHL e2 = expr { mk ~loc:($symbolstartofs,$endofs) (EBinop ("<<",e1,e2)) }
  | e1 = expr FPLUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("+.", e1, e2)) }
  | e1 = expr FMINUS e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("-.", e1, e2)) }
  | e1 = expr FTIMES e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("*.", e1, e2)) }
  | e1 = expr FDIV e2 = expr { mk ~loc:($symbolstartofs,$endofs)  (EBinop ("/.", e1, e2)) }
  /* | s=subtractive e=expr %prec prec_unary_minus { mkuminus s e } */
  | a = LID LBRACKET i=expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (EArr (a,i)) }
  /* | a = LID DOT f = LID */
  /*     { mk_expr (ERecord (a,f)) } */
  /* | a = LID LBRACKET i1=expr COLON i2=expr RBRACKET  */
  /*     { mk_expr (EBitrange (a,i1,i2)) } */
  | e1 = expr QMARK e2 = expr COLON e3 = expr
      { mk ~loc:($symbolstartofs,$endofs) (ECond (e1, e2, e3)) }
  /* | e = expr COLONCOLON t = type_expr */
  /*     { mk_expr (ECast (e, t)) } */

simple_expr:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar v) }
  | c = UID { mk ~loc:($symbolstartofs,$endofs) (ECon0 c) }
  | e = scalar_const { e }
  | LPAREN e = expr RPAREN { e }

lhs:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (LhsVar v) }
  | id = LID LBRACKET idx = expr RBRACKET { mk ~loc:($symbolstartofs,$endofs) (LhsArrInd (id, idx)) }

param_value:
  | v = scalar_const { v }
  | v = array_const { v }
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar v) }

scalar_const:
  | c = int_const { mk ~loc:($symbolstartofs,$endofs) (EInt c) }
  | c = bool_const { mk ~loc:($symbolstartofs,$endofs) (EBool c) }
  | c = float_const { mk ~loc:($symbolstartofs,$endofs) (EFloat c) }

const:
  | c = scalar_const { c }
  | c = array_const { c }
  /* | c = record_const { c } */

int_const:
  | v = INT { v }
  | MINUS v = INT { -v }

bool_const:
  | v = BOOL { v }

float_const:
  | v = FLOAT { v }

array_const:
  | LBRACKET vs = separated_nonempty_list(COMMA,const) RBRACKET
      { mk ~loc:($symbolstartofs,$endofs) (EArrExt vs) }
