%token <bool> BOOL
%token PLUS MINUS TIMES DIV
%token NOTEQUAL GT LT
%token RPAREN LPAREN
%token TYARRAY
%token LBRACKET RBRACKET

%left EQUAL NOTEQUAL GT LT
%left PLUS MINUS 
%left TIMES DIV