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
%token <char> CHAR
%token TYBOOL
%token TYINT
%token TYFLOAT
%token TYCHAR
%token TYEVENT
%token TYARRAY
%token ENUM
%token RECORD
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

let mk_type_decl p desc = { Syntax.td_desc = desc; Syntax.td_loc = mk_location p; Syntax.td_typ = Types.no_type }
let mk_fn_decl p desc = { Syntax.fd_desc = desc; Syntax.fd_loc = mk_location p; Syntax.fd_typ = Types.no_type }
let mk_cst_decl p desc = { Syntax.cst_desc = desc; Syntax.cst_loc = mk_location p }
let mk_global_decl p desc = { Syntax.mg_dsc = desc; Syntax.mg_loc = mk_location p }
let mk_fsm_decl p desc = { Syntax.fsm_desc = desc; Syntax.fsm_loc = mk_location p }
let mk_stim_decl p desc = { Syntax.stim_desc = desc; Syntax.stim_loc = mk_location p }
let mk_fsm_inst p desc = { Syntax.fi_desc = desc; Syntax.fi_loc = mk_location p }
let mk_type_expr desc = { Type_expr.te_desc = desc; Type_expr.te_typ = Types.new_type_var () }
let mk_type_expression p desc = { Syntax.te_desc = desc; Syntax.te_loc = mk_location p; Syntax.te_typ = Types.no_type }
let mk_expr desc = { Expr.e_desc = desc; Expr.e_typ = Types.new_type_var () }
let mk_expression p desc = { Syntax.e_desc = desc; Syntax.e_loc = mk_location p; Syntax.e_typ = Types.no_type }
let mk_condition p desc = { Syntax.cond_desc = desc; Syntax.cond_loc = mk_location p }
let mk_action p desc = { Syntax.act_desc = desc; Syntax.act_loc = mk_location p }

let mkuminus name exp =
  let open Expr in
  match name, exp.e_desc with
  | "-", EInt n -> { exp with e_desc = EInt (-n) }
  | ("-."|"-"), EFloat n -> { exp with e_desc = EFloat (-.n) }
  | _ -> { exp with e_desc = EFapp ("~"^name, [exp]) }

let is_type_decl = function Syntax.TypeDecl _ -> true | _ ->  false
let is_cst_decl = function Syntax.CstDecl _ -> true | _ -> false
let is_fn_decl = function Syntax.FnDecl _ -> true | _ -> false
let is_fsm_model_decl = function Syntax.FsmModelDecl _ -> true | _ -> false
let is_fsm_inst_decl = function Syntax.FsmInstDecl _ -> true | d -> false
let is_global_decl = function Syntax.GlobalDecl _ -> true | _ -> false

let type_decl_of = function Syntax.TypeDecl d -> d | _ -> Misc.fatal_error "Main_parser.type_decl_of"
let cst_decl_of = function Syntax.CstDecl d -> d  | _ -> Misc.fatal_error "Main_parser.type_decl_of"
let fn_decl_of = function Syntax.FnDecl d -> d  | _ -> Misc.fatal_error "Main_parser.type_decl_of"
let fsm_model_decl_of = function Syntax.FsmModelDecl d -> d | _ -> Misc.fatal_error "Main_parser.type_decl_of"
let fsm_inst_decl_of = function Syntax.FsmInstDecl d -> d | _ -> Misc.fatal_error "Main_parser.type_decl_of"
let globals_decl_of = function Syntax.GlobalDecl
    d -> Syntax.split_global_mdecl d
  | _ -> Misc.fatal_error "Main_parser.type_decl_of"
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

(* PROGRAM *)

program:
  | decls = my_list(decl) EOF
    { { Syntax.p_type_decls = decls |> List.filter is_type_decl |> List.map type_decl_of;
        Syntax.p_cst_decls = decls |> List.filter is_cst_decl |> List.map cst_decl_of;
        Syntax.p_fn_decls = decls |> List.filter is_fn_decl |> List.map fn_decl_of;
        Syntax.p_fsm_models = decls |> List.filter is_fsm_model_decl |> List.map fsm_model_decl_of;
        Syntax.p_fsm_insts = decls |> List.filter is_fsm_inst_decl |> List.map fsm_inst_decl_of;
        Syntax.p_globals = decls |> List.filter is_global_decl |> List.map globals_decl_of |> List.flatten }
      }
  
(* DECLARATIONS *)

decl:
  | d = type_decl { TypeDecl d }
  | d = cst_decl { CstDecl d }
  | d = fn_decl { FnDecl d }
  | d = fsm_model { FsmModelDecl d }
  | d = fsm_inst { FsmInstDecl d }
  | d = global { GlobalDecl d }

(* TYPE DECLARATION *)

type_decl:
  | TYPE id=LID EQUAL t=type_expr
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Alias (id,t)) }
  | TYPE id=LID EQUAL ENUM cs=braced(my_separated_list(COMMA,UID))
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Enum (id,cs)) }
  | TYPE id=LID EQUAL RECORD LBRACE fs=my_separated_nonempty_list(COMMA,record_field) RBRACE
      { mk_type_decl ($symbolstartofs,$endofs) (Syntax.TD_Record (id,fs)) }

record_field:
  | n=LID COLON t=type_expr { (n,t) }

(* CONSTANT DECLARATION *)

cst_decl:
  | CONSTANT name=LID COLON ty=fres EQUAL v=const
     { mk_cst_decl
         ($symbolstartofs,$endofs)
         { Syntax.cc_name=name;
           Syntax.cc_typ=ty;
           Syntax.cc_val=v; } }

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
  | VARS COLON vars=terminated(separated_list(COMMA, var),SEMICOLON) { List.flatten vars }

var:
  | ids=my_separated_nonempty_list(COMMA,LID) COLON ty=type_expr
      { List.map (fun id -> (id, mk_type_expression ($symbolstartofs,$endofs) ty)) ids }

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
  | v=LID { Action.LhsVar v }
  | a=LID LBRACKET i=expr RBRACKET { Action.LhsArrInd (a, i) }
  | a=LID LBRACKET hi=expr COLON lo=expr RBRACKET { Action.LhsArrRange (a,hi,lo) }
  | a=LID DOT f=LID { Action.LhsRField (a, f) }

(* GLOBALS *)

global:
  | INPUT id=id COLON ty=type_expr EQUAL st=stimuli
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.mg_names = [id];
           Syntax.mg_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.mg_desc = Syntax.GInp st } }
  | OUTPUT ids=my_separated_nonempty_list(COMMA,id) COLON ty=type_expr
      { mk_global_decl
            ($symbolstartofs,$endofs)
            { Syntax.mg_names = ids;
              Syntax.mg_type = mk_type_expression ($symbolstartofs,$endofs) ty;
              Syntax.mg_desc = Syntax.GOutp } }
  | SHARED ids=my_separated_nonempty_list(COMMA,id) COLON ty=type_expr
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.mg_names = ids;
           Syntax.mg_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.mg_desc = Syntax.GShared } }

stimuli:
  | PERIODIC LPAREN p=INT COMMA s=INT COMMA d=INT RPAREN
      { mk_stim_decl ($symbolstartofs,$endofs) (Global.Periodic(p,s,d)) }
  | SPORADIC ts=paren(separated_list(COMMA,INT))
      { mk_stim_decl ($symbolstartofs,$endofs) (Global.Sporadic(ts)) }
  | VALUE_CHANGES vcs=paren(separated_list(COMMA,value_change))
      { mk_stim_decl ($symbolstartofs,$endofs) (Global.ValueChange(vcs)) }
  
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
  | v = scalar_const { mk_expr (Expr.of_value v) }
  | v = array_const { mk_expr (Expr.of_value v) }
  | v = LID { mk_expr (Expr.EVar v) }

(* TYPE EXPRESSIONs *)

type_expr:
  | TYEVENT { mk_type_expr (Type_expr.TEEvent) }
  | TYINT a=int_annot { mk_type_expr (Type_expr.TEInt a) }
  | TYFLOAT { mk_type_expr (Type_expr.TEFloat) }
  | TYCHAR { mk_type_expr (Type_expr.TEChar) }
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
  | a = LID DOT f = LID
      { mk_expr (ERecord (a,f)) }
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
  | c = CHAR
      { Expr.EChar c }
  (* | c = bool
   *     { Expr.EBool c } *)

subtractive:
  | MINUS                                       { "-" }
  | FMINUS                                      { "-." }

const:
  | c = scalar_const { c }
  | c = array_const { c }
  | c = record_const { c }

array_const:
  | LBRACKET vs = separated_nonempty_list(COMMA,const) RBRACKET
      { Expr.mk_array vs }

record_const:
  | LBRACE vs = separated_nonempty_list(COMMA,record_field_const) RBRACE
      { Expr.mk_record (Types.new_name_var()) (List.map (function (n,v) -> (n, Types.new_type_var(), v)) vs) }

record_field_const:
  | id = LID EQUAL v = scalar_const { (id, v) }
                         
scalar_const:
  | v = int_const { Expr.mk_int v }
  | v = float_const { Expr.mk_float v }
  | c = char_const { Expr.mk_char c }
  (* | v = bool { Expr.Val_bool v } *)
  | c = UID { Expr.mk_val (Types.new_type_var ()) (Expr.Val_enum c) }

int_const:
  | v = INT { v }
  | MINUS v = INT { -v }

float_const:
  | v = FLOAT { v }
  | MINUS v = FLOAT { -.v }

char_const:
  | v = CHAR { v }

(* bool:
 *   | TRUE { true }
 *   | FALSE { false } *)

id:
  | i = LID { i }
  | i = UID { i }
