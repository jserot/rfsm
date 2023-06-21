%token <bool> BOOL
%token <float> FLOAT
%token PLUS MINUS TIMES DIV MOD
%token FPLUS FMINUS FTIMES FDIV
%token LAND LOR LXOR
%token SHR SHL
%token NOTEQUAL
%token RPAREN LPAREN
%token LBRACKET RBRACKET
%token TYPE
%token ENUM
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

