%token <string> LID
%token <int> INT
%token VAR
%token TYPE
%token COLON
%token COLEQ
%token PLUS MINUS TIMES DIV
%token EQUAL NOTEQUAL GT LT
%token ENUM
%token TYARRAY
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COMMA
%token <bool> BOOL
%token <string> UID

%left EQUAL NOTEQUAL GT LT
%left PLUS MINUS 
%left TIMES DIV
