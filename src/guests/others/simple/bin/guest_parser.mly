(* The parser for the guest language *)

%token <bool> BOOL
%token <float> FLOAT
%token <char> CHAR
%token PLUS MINUS TIMES DIV MOD
%token FPLUS FMINUS FTIMES FDIV
%token LAND LOR LXOR
%token SHR SHL
%token NOTEQUAL
%token LBRACKET RBRACKET
%token ENUM
%token RECORD
%token QMARK
%token LTE
%token GTE
%token COLONCOLON
%token TYINT
%token TYARRAY

%nonassoc QMARK               (* Lowest precedence *)
%left EQUAL NOTEQUAL GT LT GTE LTE
%left SHR SHL
%left LAND LOR LXOR
%left PLUS MINUS
%left TIMES DIV MOD
%left FPLUS FMINUS
%left FTIMES FDIV
%nonassoc COLONCOLON
%nonassoc prec_unary_minus         (* Highest precedence *)

%{
open Simple.Top.Syntax

let mk_binop (op,e1,e2) = EBinop (Rfsm.Ident.mk ~scope:Rfsm.Ident.Global op, e1, e2)
%}

%%
%public type_decl:
  | TYPE id=LID EQUAL ENUM LBRACE ctors=separated_nonempty_list(COMMA,ctor) RBRACE
     { mk ~loc:$sloc (TD_Enum (mk_global_ident id,ctors)) }
  | TYPE id=LID EQUAL RECORD LBRACE fs=separated_nonempty_list(COMMA,record_field) RBRACE
      { mk ~loc:$sloc (TD_Record (mk_global_ident id,fs)) }
  | TYPE id=LID EQUAL t=type_expr
      { mk ~loc:$sloc (TD_Alias (mk_global_ident id,t)) }

record_field:
  | n=LID COLON t=type_expr { (n,t) }

ctor:
  | c = UID { c }

%public type_expr:
  | TYINT
    { mk ~loc:$sloc (TeConstr (mk_global_ident "int", [], None)) }
  | t=type_expr TYARRAY LBRACKET sz=array_size RBRACKET
    { mk ~loc:$sloc (TeConstr (mk_global_ident "array", [t], Some sz)) }
  | c = LID
    { mk ~loc:$sloc (TeConstr (mk_global_ident c, [], None)) }

array_size:
  | c = INT { c }

%public expr:
  | e = simple_expr { e }
  | e1 = expr PLUS e2 = expr { mk ~loc:$sloc  (mk_binop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr { mk ~loc:$sloc  (mk_binop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr { mk ~loc:$sloc  (mk_binop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr { mk ~loc:$sloc  (mk_binop ("/", e1, e2)) }
  | e1 = expr MOD e2 = expr { mk ~loc:$sloc  (mk_binop ("%", e1, e2)) }
  | e1 = expr EQUAL e2 = expr { mk ~loc:$sloc (mk_binop ("=",e1,e2)) }
  | e1 = expr NOTEQUAL e2 = expr { mk ~loc:$sloc (mk_binop ("!=",e1,e2)) }
  | e1 = expr GT e2 = expr { mk ~loc:$sloc (mk_binop (">",e1,e2)) }
  | e1 = expr LT e2 = expr { mk ~loc:$sloc (mk_binop ("<",e1,e2)) }
  | e1 = expr GTE e2 = expr { mk ~loc:$sloc (mk_binop (">=",e1,e2)) }
  | e1 = expr LTE e2 = expr { mk ~loc:$sloc (mk_binop ("<=",e1,e2)) }
  | e1 = expr LAND e2 = expr { mk ~loc:$sloc (mk_binop ("&",e1,e2)) }
  | e1 = expr LOR e2 = expr { mk ~loc:$sloc (mk_binop ("|",e1,e2)) }
  | e1 = expr LXOR e2 = expr { mk ~loc:$sloc (mk_binop ("^",e1,e2)) }
  | e1 = expr SHR e2 = expr { mk ~loc:$sloc (mk_binop (">>",e1,e2)) }
  | e1 = expr SHL e2 = expr { mk ~loc:$sloc (mk_binop ("<<",e1,e2)) }
  | e1 = expr FPLUS e2 = expr { mk ~loc:$sloc  (mk_binop ("+.", e1, e2)) }
  | e1 = expr FMINUS e2 = expr { mk ~loc:$sloc  (mk_binop ("-.", e1, e2)) }
  | e1 = expr FTIMES e2 = expr { mk ~loc:$sloc  (mk_binop ("*.", e1, e2)) }
  | e1 = expr FDIV e2 = expr { mk ~loc:$sloc  (mk_binop ("/.", e1, e2)) }
  | s=subtractive e=expr %prec prec_unary_minus { mkuminus s e }
  | a = LID LBRACKET i=expr RBRACKET { mk ~loc:$sloc (EIndexed (mk_ident a,i)) }
  | f = LID LPAREN args=separated_list(COMMA,expr) RPAREN { mk ~loc:$sloc (EFapp (mk_global_ident f,args)) }
  | a = LID DOT f = LID { mk ~loc:$sloc (ERecord (mk_ident a,f)) }
  | a = LID LBRACKET i1=expr COLON i2=expr RBRACKET { mk ~loc:$sloc (ERanged (mk_ident a,i1,i2)) }
  | e1 = expr QMARK e2 = expr COLON e3 = expr { mk ~loc:$sloc (ECond (e1, e2, e3)) }
  | e = expr COLONCOLON t = type_expr { mk ~loc:$sloc (ECast (e,t)) }

simple_expr:
  | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) }
  | c = UID { mk ~loc:$sloc (ECon0 (mk_global_ident c)) }
  | e = scalar_const { e }
  | LPAREN e = expr RPAREN { e }

%public lval:
  | v = LID { mk ~loc:$sloc (LvalVar (mk_ident v)) }
  | id = LID LBRACKET idx = expr RBRACKET { mk ~loc:$sloc (LvalIndex (mk_ident id, idx)) }
  | a=LID DOT f=LID { mk ~loc:$sloc (LvalRField (mk_ident a, f)) }
  | a=LID LBRACKET hi=expr COLON lo=expr RBRACKET { mk ~loc:$sloc (LvalRange (mk_ident a,hi,lo)) }

%public param_value:
  | v = scalar_const { v }
  | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) }

%public scalar_const:
  | c = INT { mk ~loc:$sloc (EInt c) }
  | MINUS c = INT { mk ~loc:$sloc (EInt (-c)) }
  | c = BOOL { mk ~loc:$sloc (EBool c) }
  | c = FLOAT { mk ~loc:$sloc (EFloat c) }
  | MINUS c = FLOAT { mk ~loc:$sloc (EFloat (-.c)) }
  | c = CHAR { mk ~loc:$sloc (EChar c) }

%public const:
  | c = scalar_const { c }
  | c = array_const { c }

%public stim_const: 
  | c = scalar_const { c }
  | c = UID { mk ~loc:$sloc (ECon0 (mk_global_ident c)) }
  | c = record_const { mk ~loc:$sloc c }

record_const:
  | LBRACE vs = separated_nonempty_list(COMMA,record_field_const) RBRACE { ERecordExt vs }

record_field_const:
  | id = LID EQUAL v = stim_const { (id, v) }
  
subtractive:
  | MINUS                                       { "-" }
  | FMINUS                                      { "-." }

array_const:
  | LBRACKET vs = separated_nonempty_list(COMMA,const) RBRACKET
      { mk ~loc:$sloc (EArrExt vs) }
