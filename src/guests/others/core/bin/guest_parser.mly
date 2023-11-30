(* The parser for the guest language *)
(* See file ../../../../host/lib/host_parser.mly for the list of keywords already defined by the host language *)

%token <bool> BOOL
%token PLUS MINUS TIMES DIV
%token NOTEQUAL

(* Precedences and associativities for expressions *)

%left EQUAL NOTEQUAL GT LT
%left PLUS MINUS 
%left TIMES DIV

%{
open Core.Top.Syntax

let mk_binop (op,e1,e2) = EBinop (Rfsm.Ident.mk ~scope:Rfsm.Ident.Global op, e1, e2)
%}

%%

%public type_decl:
 | TYPE /* Not used. There's no type declaration in the Core guest language */ { mk ~loc:$sloc () }

%public type_expr:
  | tc = LID { mk ~loc:$sloc (TeConstr tc) }

%public expr:
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

%public lval:
  | v = LID { mk ~loc:$sloc (LvalVar (mk_ident v)) }

%public param_value:
  | v = scalar_const { v }

%public scalar_const:
  | c = INT { mk ~loc:$sloc (EInt c) }
  | MINUS c = INT { mk ~loc:$sloc (EInt (-c)) }
  | c = BOOL { mk ~loc:$sloc (EBool c) }

%public const:
  | c = scalar_const { c }

%public stim_const: 
  | c = scalar_const { c }

