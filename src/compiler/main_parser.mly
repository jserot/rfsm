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
%token TYBOOL
%token TYINT
%token TYEVENT
%token TRUE
%token FALSE
%token <string> ID
(* %token <string> STRING *)
%token SEMICOLON
%token COMMA
%token DOT
%token COLON
%token COLEQ
%token EQUAL
%token NOTEQUAL
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LT
%token GT
%token LTE
%token GTE
%token PLUS MINUS TIMES DIV MOD
%token DOTDOT
%token ARROW_START
%token ARROW_END
%token BAR
(* %token BARBAR *)
%token EOF

(* Precedences and associativities for expressions *)

%left PLUS MINUS       
%left TIMES DIV MOD   
%nonassoc UMINUS          (* Highest precedence *)

%type <Syntax.program> program
(* %type <Condition.t> fsm_condition *)
(* %type <Action.t list> fsm_actions *)

%start program (*fsm_condition fsm_actions*)

%{
open Location

let mk_location (p1,p2) = Loc (!input_name, p1, p2)

let mk_global_decl p desc = { Syntax.g_desc = desc; Syntax.g_loc = mk_location p }
let mk_fsm_decl p desc = { Syntax.fsm_desc = desc; Syntax.fsm_loc = mk_location p }
let mk_stim_decl p desc = { Syntax.stim_desc = desc; Syntax.stim_loc = mk_location p }
let mk_fsm_inst p desc = { Syntax.fi_desc = desc; Syntax.fi_loc = mk_location p }
let mk_type_expression p desc = { Syntax.te_desc = desc; Syntax.te_loc = mk_location p }
let mk_type_index_expression p desc = { Syntax.ti_desc = desc; Syntax.ti_loc = mk_location p }
let mk_expression p desc = { Syntax.e_desc = desc; Syntax.e_loc = mk_location p }
let mk_condition p desc = { Syntax.cond_desc = desc; Syntax.cond_loc = mk_location p }
let mk_action p desc = { Syntax.act_desc = desc; Syntax.act_loc = mk_location p }

(* let negate_expr = function
 *     Expr.EConst c -> Expr.EConst (-c)
 *   | e -> Expr.EBinop ("-", Expr.EConst 0, e) *)
%}

%%

%public braced(X):
  | LBRACE x=X RBRACE { x }

%public paren(X):
  | LPAREN x=X RPAREN { x }

%public optional(X):
    /* Nothing */ { [] }
  | x=X { x }

program:
  | models=nonempty_list(fsm_model)
    globals=nonempty_list(global)
    fsms=nonempty_list(fsm_inst)
    EOF
      { { Syntax.p_fsm_models = models;
          Syntax.p_globals = globals;
          Syntax.p_fsm_insts = fsms; }
      }
  
(* FSM MODEL *)

fsm_model:
  | FSM MODEL
      name=ID 
      params=optional(params)
      LPAREN ios=separated_list(COMMA, io) RPAREN
      LBRACE
      STATES COLON states=terminated(separated_list(COMMA, ID),SEMICOLON)
      vars = optional(vars)
      TRANS COLON trans=terminated(separated_list(COMMA,transition),SEMICOLON)
      ITRANS COLON itrans=terminated(itransition,SEMICOLON)
      RBRACE {
        mk_fsm_decl
          ($symbolstartofs,$endofs)
          { Syntax.fd_name=name;
            Syntax.fd_params=params;
            Syntax.fd_states=states;
            Syntax.fd_ios=ios;
            (* Syntax.fd_inps=collect_io IO_In ios; *)
            (* Syntax.fd_outps=collect_io IO_Out ios; *)
            (* Syntax.fd_inouts=collect_io IO_InOut ios; *)
            Syntax.fd_vars=vars;
            Syntax.fd_trans=trans;
            Syntax.fd_itrans=itrans } }

params:
  | LT params=separated_list(COMMA, param) GT { params }

param:
  | id=ID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

io:
  | IN d=io_desc { (Types.IO_In, d) }
  | OUT d=io_desc { (Types.IO_Out, d) }
  | INOUT d=io_desc { (Types.IO_Inout, d) }

io_desc:
  | id=ID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

vars:
  | VARS COLON vars=terminated(separated_list(COMMA, var),SEMICOLON) { vars }

var:
  | id=ID COLON ty=typ { (id, mk_type_expression ($symbolstartofs,$endofs) ty) }

transition:
  | src=ID
    ARROW_START
    cond=condition
    actions=optional(actions)
    ARROW_END
    dst=ID
      { src, mk_condition ($symbolstartofs,$endofs) cond, actions, dst }

itransition:
  | actions=optional(actions) ARROW_END dst=ID { dst, actions }

condition:
  | ev=ID { ([ev],[]) }
  | ev=ID DOT guards=separated_nonempty_list(DOT, guard) { ([ev], guards) }

guard:
  | e=rel_expr { e }

actions:
  | BAR actions=separated_nonempty_list(SEMICOLON, action) { actions }

action:
  | i=ID               { mk_action ($symbolstartofs,$endofs) (Action.Emit i) }
  | i=ID COLEQ e=expr  { mk_action ($symbolstartofs,$endofs) (Action.Assign (i,e)) }

(* GLOBALS *)

global:
  | INPUT id=ID COLON ty=typ EQUAL st=stimuli
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GInp st } }
  | OUTPUT id=ID COLON ty=typ
      { mk_global_decl
         ($symbolstartofs,$endofs)
         { Syntax.gd_name = id;
           Syntax.gd_type = mk_type_expression ($symbolstartofs,$endofs) ty;
           Syntax.gd_desc = Syntax.GOutp } }
  | SHARED id=ID COLON ty=typ
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
  | FSM name=ID EQUAL model=ID pvs=opt_inst_params args=paren(separated_list(COMMA,ID))
      { mk_fsm_inst
          ($symbolstartofs,$endofs)
          { Syntax.fi_name = name;
            Syntax.fi_model = model;
            Syntax.fi_params = pvs;
            Syntax.fi_args = args }  }

opt_inst_params:
    /* Nothing */ { [] }
  |  LT params=separated_nonempty_list(COMMA, INT) GT { List.map (function v -> Expr.Val_int v) params }
  
(* TYPE EXPRESSIONs *)

typ:
  | TYEVENT { Syntax.TEEvent }
  | TYINT r=option(int_range) { Syntax.TEInt r }
  | TYBOOL { Syntax.TEBool }

int_range:
    | LT lo=type_index_expr DOTDOT hi=type_index_expr GT
        { (mk_type_index_expression ($symbolstartofs,$endofs) lo,
           mk_type_index_expression ($symbolstartofs,$endofs) hi) }

type_index_expr:
  | c = int
      { Syntax.TEConst c }
  | i = ID
      { Syntax.TEVar i }
  | LPAREN e = type_index_expr RPAREN
      { e }
  | e1 = type_index_expr PLUS e2 = type_index_expr
      { Syntax.TEBinop ("+", e1, e2) }
  | e1 = type_index_expr MINUS e2 = type_index_expr
      { Syntax.TEBinop ("-", e1, e2) }
  | e1 = type_index_expr TIMES e2 = type_index_expr
      { Syntax.TEBinop ("*", e1, e2) }
  | e1 = type_index_expr DIV e2 = type_index_expr
      { Syntax.TEBinop ("/", e1, e2) }
  | e1 = type_index_expr MOD e2 = type_index_expr
      { Syntax.TEBinop ("mod", e1, e2) }

(* GUARD EXPRESSIONS *)

rel_expr:
  | e1 = expr EQUAL e2 = expr
      { (e1, "=", e2) }
  | e1 = expr NOTEQUAL e2 = expr
      { (e1, "!=", e2) }
  | e1 = expr GT e2 = expr
      { (e1, ">", e2) }
  | e1 = expr LT e2 = expr
      { (e1, "<", e2) }
  | e1 = expr GTE e2 = expr
      { (e1, ">=", e2) }
  | e1 = expr LTE e2 = expr
      { (e1, "<=", e2) }

expr:
  | c = int
      { Expr.EInt c }
  | c = bool
      { Expr.EBool c }
  | i = ID
      { Expr.EVar i }
  | LPAREN e = expr RPAREN
      { e }
  | e1 = expr PLUS e2 = expr
      { Expr.EBinop ("+", e1, e2) }
  | e1 = expr MINUS e2 = expr
      { Expr.EBinop ("-", e1, e2) }
  | e1 = expr TIMES e2 = expr
      { Expr.EBinop ("*", e1, e2) }
  | e1 = expr DIV e2 = expr
      { Expr.EBinop ("/", e1, e2) }
  | e1 = expr MOD e2 = expr
      { Expr.EBinop ("mod", e1, e2) }

const:
  | v = int { Expr.Val_int v }
  | v = bool { Expr.Val_bool v }

int:
  | c = INT { c }
  | MINUS c = INT %prec UMINUS { -c }

bool:
  | TRUE { true }
  | FALSE { false }

(* Separate, "standalone"  entries for string parsers *)

(* fsm_condition:  *)
(*   | cond = option(condition) EOF { mk_cond cond } *)

(* fsm_actions:  *)
(*   | acts = action_list EOF { acts } *)

