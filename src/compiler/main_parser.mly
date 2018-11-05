%token TYPE
%token FUNCTION
%token CONSTANT
%token RETURN
%token FSM
%token MODEL
%token IN
%token OUT
%token INOUT
%token PERIODIC
%token SPORADIC
%token VALUE_CHANGES
%token SHARED
%token STATES
%token INPUT
%token OUTPUT
%token VARS
%token TRANS
%token ITRANS
%token <int> INT
%token <float> FLOAT
%token TYBOOL
%token TYINT
%token TYFLOAT
%token TYEVENT
%token TYARRAY
(* %token TRUE
 * %token FALSE *)
%token <string> LID
%token <string> UID
(* %token <string> STRING *)
%token SEMICOLON
%token COMMA
%token DOT
%token COLON
%token QMARK
%token COLEQ
%token EQUAL
%token NOTEQUAL
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LT
%token GT
%token LTE
%token GTE
%token PLUS MINUS TIMES DIV MOD
%token FPLUS FMINUS FTIMES FDIV
%token LAND LOR LXOR
%token SHL SHR
%token ARROW_START
%token ARROW_END
%token BAR
%token COLONCOLON
(* %token BARBAR *)
%token EOF

(* Precedences and associativities for expressions *)

%nonassoc QMARK COLON              (* Lowest precedence *)
%left EQUAL NOTEQUAL GT LT GTE LTE
%left SHR SHL
%left LAND LOR LXOR
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV MOD   
%nonassoc COLONCOLON
%nonassoc prec_unary_minus         (* Highest precedence *)

%type <Syntax.program> program

%start program

%{
open Location

let mk_location (p1,p2) = Loc (!input_name, p1, p2)

let mk_type_decl p desc = { Syntax.td_desc = desc; Syntax.td_loc = mk_location p }
let mk_fn_decl p desc = { Syntax.fd_desc = desc; Syntax.fd_loc = mk_location p }
let mk_cst_decl p desc = { Syntax.cst_desc = desc; Syntax.cst_loc = mk_location p }
let mk_global_decl p desc = { Syntax.g_desc = desc; Syntax.g_loc = mk_location p }
let mk_fsm_decl p desc = { Syntax.fsm_desc = desc; Syntax.fsm_loc = mk_location p }
let mk_stim_decl p desc = { Syntax.stim_desc = desc; Syntax.stim_loc = mk_location p }
let mk_fsm_inst p desc = { Syntax.fi_desc = desc; Syntax.fi_loc = mk_location p }
let mk_type_expr desc = { Type_expr.te_desc = desc; Type_expr.te_typ = TyUnknown }
let mk_type_expression p desc = { Syntax.te_desc = desc; Syntax.te_loc = mk_location p }
let mk_expr desc = { Expr.e_desc = desc; Expr.e_typ = TyUnknown }
let mk_expression p desc = { Syntax.e_desc = desc; Syntax.e_loc = mk_location p }
let mk_condition p desc = { Syntax.cond_desc = desc; Syntax.cond_loc = mk_location p }
let mk_action p desc = { Syntax.act_desc = desc; Syntax.act_loc = mk_location p }

let mkuminus name exp =
  let open Expr in
  match name, exp.e_desc with
  | "-", EInt n -> { exp with e_desc = EInt (-n) }
  | ("-."|"-"), EFloat n -> { exp with e_desc = EFloat (-.n) }
  | _ -> { exp with e_desc = EFapp ("~"^name, [exp]) }
%}

%%

%public braced(X):
  | LBRACE x=X RBRACE { x }

%public paren(X):
  | LPAREN x=X RPAREN { x }

%public optional(X):
    /* Nothing */ { [] }
  | x=X { x }

%public my_list(X):
  /* nothing */
    { [] }
| x = X; xs = my_list(X)
    { x :: xs }

%public my_nonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = my_nonempty_list(X)
    { x :: xs }

%public my_separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = my_separated_nonempty_list(separator, X)
    { x :: xs }

%public my_separated_list(separator, X):
  /* nothing */
    { [] }
| x = my_separated_nonempty_list(separator, X)
    { x }

program:
  | tydecls=my_list(type_decl)
    cstdecls=my_list(cst_decl)
    fndecls=my_list(fn_decl)
    models=my_nonempty_list(fsm_model)
    globals=my_nonempty_list(global)
    fsms=my_nonempty_list(fsm_inst)
    EOF
    { { Syntax.p_type_decls = tydecls;
        Syntax.p_cst_decls = cstdecls;
        Syntax.p_fn_decls = fndecls;
        Syntax.p_fsm_models = models;
        Syntax.p_globals = globals;
        Syntax.p_fsm_insts = fsms; }
      }
  
(* TYPE DECLARATION *)

type_decl:
  | TYPE id=LID EQUAL t=type_expr
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Alias (id,t)) }
  | TYPE id=LID EQUAL cs=braced(my_separated_list(COMMA,UID))
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Enum (id,cs)) }

(* CONSTANT DECLARATION *)

cst_decl:
  | CONSTANT name=LID COLON ty=fres EQUAL v=const_val
     { mk_cst_decl
         ($symbolstartofs,$endofs)
         { Syntax.cc_name=name;
           Syntax.cc_typ=ty;
           Syntax.cc_val=v; } }

const_val:  
  | v=int_const { Expr.Val_int v }
  | v=float_const { Expr.Val_float v }
  | v=const_array_val { Expr.Val_array v }

const_array_val:
  | LBRACKET vs = separated_nonempty_list(COMMA,const_val) RBRACKET { Array.of_list vs }

(* FUNCTION DECLARATION *)

fn_decl:
  | FUNCTION
     name=LID
     LPAREN args=my_separated_list(COMMA, farg) RPAREN
     COLON res=fres
     LBRACE RETURN body=fbody RBRACE
     { mk_fn_decl
         ($symbolstartofs,$endofs)
         { Syntax.ff_name=name;
           Syntax.ff_args=args;
           Syntax.ff_res=res;
           Syntax.ff_body=body; } }
farg:
  | id=LID COLON ty=type_expr { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

fres:
  | ty=type_expr { mk_type_expression ($symbolstartofs,$endofs) ty }

fbody:
  | e=expr { mk_expression ($symbolstartofs,$endofs) e }

(* FSM MODEL *)

fsm_model:
  | FSM MODEL
      name=id 
      params=optional(params)
      LPAREN ios=my_separated_list(COMMA, io) RPAREN
      LBRACE
      STATES COLON states=terminated(my_separated_list(COMMA, UID),SEMICOLON)
      vars = optional(vars)
      TRANS COLON trans=terminated(my_separated_list(COMMA,transition),SEMICOLON)
      ITRANS COLON itrans=terminated(itransition,SEMICOLON)
      RBRACE {
        mk_fsm_decl
          ($symbolstartofs,$endofs)
          { Syntax.fd_name=name;
            Syntax.fd_params=params;
            Syntax.fd_states=states;
            Syntax.fd_ios=ios;
            Syntax.fd_vars=vars;
            Syntax.fd_trans=trans;
            Syntax.fd_itrans=itrans } }

params:
  | LT params=my_separated_list(COMMA, param) GT { params }

param:
  | id=LID COLON ty=type_expr { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

io:
  | IN d=io_desc { (Types.IO_In, d) }
  | OUT d=io_desc { (Types.IO_Out, d) }
  | INOUT d=io_desc { (Types.IO_Inout, d) }

io_desc:
  | id=LID COLON ty=type_expr { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

vars:
  | VARS COLON vars=terminated(separated_list(COMMA, var),SEMICOLON) { vars }

var:
  | id=LID COLON ty=type_expr { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

transition:
  | prio=prio
    src=UID
    ARROW_START
    cond=condition
    actions=optional(actions)
    ARROW_END
    dst=UID
      { src, mk_condition ($symbolstartofs,$endofs) cond, actions, dst, prio }

prio:
    | (* Nothing *) { false }
    | TIMES { true } 
      
itransition:
  | actions=optional(actions) ARROW_END dst=UID { dst, actions }

condition:
  | ev=LID { ([ev],[]) }
  | ev=LID DOT guards=separated_nonempty_list(DOT, expr) { ([ev], guards) }

actions:
  | BAR actions=separated_nonempty_list(SEMICOLON, action) { actions }

action:
  | i=LID               { mk_action ($symbolstartofs,$endofs) (Action.Emit i) }
  | l=lhs COLEQ e=expr  { mk_action ($symbolstartofs,$endofs) (Action.Assign ({l_desc=l},e)) }

lhs:
  | v=LID { Action.Var0 v }
  | a=LID LBRACKET i=expr RBRACKET { Action.Var1 (a, i) }
  | a=LID LBRACKET hi=expr COLON lo=expr RBRACKET { Action.Var2 (a,hi,lo) }

(* GLOBALS *)

global:
  | INPUT id=id COLON ty=type_expr EQUAL st=stimuli
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GInp st } }
  | OUTPUT id=id COLON ty=type_expr
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GOutp } }
  | SHARED id=id COLON ty=type_expr
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GShared } }

stimuli:
  | PERIODIC LPAREN p=INT COMMA s=INT COMMA d=INT RPAREN
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.Periodic(p,s,d)) }
  | SPORADIC ts=paren(separated_list(COMMA,INT))
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.Sporadic(ts)) }
  | VALUE_CHANGES vcs=paren(separated_list(COMMA,value_change))
      { mk_stim_decl ($symbolstartofs,$endofs) (Syntax.ValueChange(vcs)) }
  
value_change:
  | t=INT COLON v=const { (t,v) }
  
(* INSTANCEs *)

fsm_inst:
  | FSM name=id EQUAL model=id pvs=opt_inst_params args=paren(separated_list(COMMA,id))
      { mk_fsm_inst
          ($symbolstartofs,$endofs)
          { Syntax.fi_name = name;
            Syntax.fi_model = model;
            Syntax.fi_params = pvs;
            Syntax.fi_args = args }  }

opt_inst_params:
    /* Nothing */ { [] }
  |  LT params=separated_nonempty_list(COMMA, inst_param_value) GT { params }

inst_param_value:  
  | v=int_const { Expr.Val_int v }
  | v=float_const { Expr.Val_float v }
  (* | v=bool { Expr.Val_bool v } *)
  | v=array_val { Expr.Val_array v }

array_val:
  | LBRACKET vs = separated_nonempty_list(COMMA,inst_param_value) RBRACKET { Array.of_list vs }
                   
(* TYPE EXPRESSIONs *)

type_expr:
  | TYEVENT { mk_type_expr (Type_expr.TEEvent) }
  | TYINT a=int_annot { mk_type_expr (Type_expr.TEInt a) }
  | TYFLOAT { mk_type_expr (Type_expr.TEFloat) }
  | TYBOOL { mk_type_expr (Type_expr.TEBool) }
  | i=LID { mk_type_expr (Type_expr.TEName i) }
  | t=type_expr TYARRAY LBRACKET s=array_size RBRACKET { mk_type_expr (Type_expr.TEArray (s,t)) }

int_annot:
    | (* Nothing *)
      { TA_none }
    | LT sz=type_index_expr GT
        { TA_size sz }
    | LT lo=type_index_expr COLON hi=type_index_expr GT
        { TA_range (lo, hi) }

array_size:
    | sz = type_index_expr { sz }

type_index_expr:
  | c = int_const
      { Type_expr.TEConst c }
  | i = LID
      { Type_expr.TEVar i }
  | LPAREN e = type_index_expr RPAREN
      { e }
  | e1 = type_index_expr PLUS e2 = type_index_expr
      { Type_expr.TEBinop ("+", e1, e2) }
  | e1 = type_index_expr MINUS e2 = type_index_expr
      { Type_expr.TEBinop ("-", e1, e2) }
  | e1 = type_index_expr TIMES e2 = type_index_expr
      { Type_expr.TEBinop ("*", e1, e2) }
  | e1 = type_index_expr DIV e2 = type_index_expr
      { Type_expr.TEBinop ("/", e1, e2) }
  | e1 = type_index_expr MOD e2 = type_index_expr
      { Type_expr.TEBinop ("mod", e1, e2) }

(* EXPRESSIONS *)

expr:
  | e = simple_expr
      { e }
  | e1 = expr SHR e2 = expr
      { mk_expr (EBinop (">>", e1, e2)) }
  | e1 = expr SHL e2 = expr
      { mk_expr (EBinop ("<<", e1, e2)) }
  | e1 = expr LAND e2 = expr
      { mk_expr (EBinop ("&", e1, e2)) }
  | e1 = expr LOR e2 = expr
      { mk_expr (EBinop ("|", e1, e2)) }
  | e1 = expr LXOR e2 = expr
      { mk_expr (EBinop ("^", e1, e2)) }
  | e1 = expr PLUS e2 = expr
      { mk_expr (EBinop ("+", e1, e2)) }
  | e1 = expr MINUS e2 = expr
      { mk_expr (EBinop ("-", e1, e2)) }
  | e1 = expr TIMES e2 = expr
      { mk_expr (EBinop ("*", e1, e2)) }
  | e1 = expr DIV e2 = expr
      { mk_expr (EBinop ("/", e1, e2)) }
  | e1 = expr MOD e2 = expr
      { mk_expr (EBinop ("mod", e1, e2)) }
  | e1 = expr FPLUS e2 = expr
      { mk_expr (EBinop ("+.", e1, e2)) }
  | e1 = expr FMINUS e2 = expr
      { mk_expr (EBinop ("-.", e1, e2)) }
  | e1 = expr FTIMES e2 = expr
      { mk_expr (EBinop ("*.", e1, e2)) }
  | e1 = expr FDIV e2 = expr
      { mk_expr (EBinop ("/.", e1, e2)) }
  | e1 = expr EQUAL e2 = expr
      { mk_expr (EBinop ("=", e1, e2)) }
  | e1 = expr NOTEQUAL e2 = expr
      { mk_expr (EBinop ("!=", e1, e2)) }
  | e1 = expr GT e2 = expr
      { mk_expr (EBinop (">", e1, e2)) }
  | e1 = expr LT e2 = expr
      { mk_expr (EBinop ("<", e1, e2)) }
  | e1 = expr GTE e2 = expr
      { mk_expr (EBinop (">=", e1, e2)) }
  | e1 = expr LTE e2 = expr
      { mk_expr (EBinop ("<=", e1, e2)) }
  | s=subtractive e=expr %prec prec_unary_minus
      { mkuminus s e }
  | f = LID LPAREN args=my_separated_list(COMMA,expr) RPAREN
      { mk_expr (EFapp (f,args)) }
  | a = LID LBRACKET i=expr RBRACKET 
      { mk_expr (EArr (a,i)) }
  | a = LID LBRACKET i1=expr COLON i2=expr RBRACKET 
      { mk_expr (EBitrange (a,i1,i2)) }
  | e1 = expr QMARK e2 = expr COLON e3 = expr
      { mk_expr (ECond (e1, e2, e3)) }
  | e = expr COLONCOLON t = type_expr
      { mk_expr (ECast (e, t)) }

simple_expr:
  | v = LID
      { mk_expr (Expr.EVar v) }
  | e = constant
      { mk_expr e }
  | c = UID
      { mk_expr (Expr.EEnum c) }
  | LPAREN e = expr RPAREN
      { e }

constant:
  | c = INT
      { Expr.EInt c }
  | c = FLOAT
      { Expr.EFloat c }
  (* | c = bool
   *     { Expr.EBool c } *)

subtractive:
  | MINUS                                       { "-" }
  | FMINUS                                      { "-." }

const:
  | v = int_const { Expr.Val_int v }
  | v = float_const { Expr.Val_float v }
  (* | v = bool { Expr.Val_bool v } *)
  | c = UID { Expr.Val_enum c }

int_const:
  | v = INT { v }
  | MINUS v = INT { -v }

float_const:
  | v = FLOAT { v }
  | MINUS v = FLOAT { -.v }

(* bool:
 *   | TRUE { true }
 *   | FALSE { false } *)

id:
  | i = LID { i }
  | i = UID { i }
