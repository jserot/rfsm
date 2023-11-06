/**********************************************************************/
/*                                                                    */
/*              This file is part of the RFSM package                 */
/*                                                                    */
/*  Copyright (c/ 2018-present, Jocelyn SEROT.  All rights reserved.  *)
/*                                                                    */
/*  This source code is licensed under the license found in the       */
/*  LICENSE file in the root directory of this source tree.           */
/*                                                                    */
/**********************************************************************/

/* Parser for the host language */

/* This file will be pre-processed to generate the parser for the guest language */

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
%token SEMICOLON
%token EQUAL
%token COMMA
%token DOT
%token COLON
%token COLEQ
%token LBRACE
%token RBRACE
%token ARROW
%token ON
%token WHEN
%token WITH
%token BAR
%token EMARK
%token <string> LID
%token <string> UID
%token <int> INT
%token CONSTANT
%token FUNCTION
%token RETURN
%token LT GT
%token EOF
%token WHERE
%token AND
%token RPAREN LPAREN
%token TYPE
#include "guest_tokens.mly"

%type <Lang.L.Syntax.program> program
%start program

%{
#include "guest_open.mly"

let mk_io (cat,(id,ty)) = id, (cat,ty)
let mk_io' (cat,(id,ty)) = id, ty
let mk_ident x = Rfsm.Ident.mk x
let mk_global_ident x = Rfsm.Ident.(mk ~scope:Global x)
let mk_state ~loc:l x = Annot.{ desc=x; typ=(); loc=Location.mk l }
%}

%%

%public paren(X):
  | LPAREN x=X RPAREN { x }

%public optional(X):
    /* Nothing */ { [] }
  | x=X { x }

(* PROGRAM *)

program:
  | type_decls = list(type_decl)
    cst_decls = list(cst_decl)
    fun_decls = list(fun_decl)
    models = list(fsm_model)
    globals = list(global)
    insts = list(fsm_inst) EOF
    { Lang.L.Syntax.{ 
        type_decls = type_decls;
        fun_decls = fun_decls;
        cst_decls = cst_decls;
        models = models;
        globals = List.flatten globals;
        insts = insts;
      } }
  
(* TYPE DECLS are entirely defined by the GUEST language *)

(* CONSTANT DECLS *)

cst_decl:
  | CONSTANT name=LID COLON ty=type_expr EQUAL v=const
     { mk ~loc:$sloc Lang.L.Syntax.{cc_name=mk_global_ident name; cc_typ=ty; cc_val=v} }

(* FUNCTION DECLS *)

fun_decl:
  | FUNCTION
     name=LID
     LPAREN args=separated_list(COMMA, farg) RPAREN
     COLON res=type_expr
     LBRACE RETURN body=expr RBRACE
     { mk
         ~loc:$sloc
         Lang.L.Syntax.{ ff_name=mk_global_ident name; ff_args=args; ff_res=res; ff_body=body; } }

farg:
  | id=LID COLON te=type_expr { (mk_ident id, te) }

(* FSM MODELS *)

fsm_model:
  | FSM MODEL
      name=id 
      params=optional(params)
      LPAREN ios=separated_list(COMMA, io) RPAREN
      LBRACE
      STATES COLON states=terminated(separated_list(COMMA, state),SEMICOLON)
      vars = optional(vars)
      TRANS COLON trans=terminated(list(transition),SEMICOLON)
      ITRANS COLON itrans=terminated(itransition,SEMICOLON)
      RBRACE {
        mk
          ~loc:$sloc
          Lang.L.Syntax.{ name = mk_global_ident name;
            params = params;
            states = states;
            ios = List.map mk_io ios;
            inps = ios |> List.filter (function (In,_) -> true | _ -> false) |> List.map mk_io';
            outps = ios |> List.filter (function (Out,_) -> true | _ -> false) |> List.map mk_io';
            inouts = ios |> List.filter (function (InOut,_) -> true | _ -> false) |> List.map mk_io';
            vars = vars;
            trans = trans;
            itrans = itrans } }

params:
  | LT params=separated_list(COMMA, param) GT { params }

param:
  | id=LID COLON ty=type_expr { (mk_ident id, ty) }

io:
  | IN d=io_desc { (Lang.L.Syntax.In, d) }
  | OUT d=io_desc { (Lang.L.Syntax.Out, d) }
  | INOUT d=io_desc { (Lang.L.Syntax.InOut, d) }

io_desc:
  | id=LID COLON ty=type_expr { mk_ident id,ty }

state:
  | id=UID { mk_state ~loc:$sloc (mk_ident id,[]) }
  | id=UID WHERE ovs=separated_nonempty_list(AND,outp_valuation) { mk_state ~loc:$sloc (mk_ident id,ovs) }

outp_valuation:
  | id=LID EQUAL e=scalar_const { (mk_ident id, e) }
                   
vars:
  | VARS COLON vars=terminated(separated_list(COMMA, var),SEMICOLON) { List.flatten vars }

var:
  | ids=separated_nonempty_list(COMMA,LID) COLON ty=type_expr
      { List.map (fun id -> (mk_ident id, ty)) ids }

transition:
  | p=prio src=UID ARROW dst=UID cond=cond acts=actions
      { mk ~loc:$sloc (mk_ident src, cond, acts, mk_ident dst, p) }

prio:
  | BAR { 1 }  /* Low priority */
  | EMARK { 0 }  /* High priority */

cond: 
  | ON ev=LID gds=guards
      { mk ~loc:$sloc (mk_ident ev,gds) }


itransition:
  | BAR ARROW dst=UID acts=actions
      { mk ~loc:$sloc (mk_ident dst, acts) }

guards:
  | (* Nothing *) { [] }
  | WHEN conds=separated_nonempty_list(DOT, expr) { conds }

actions:
  | (* Nothing *) { [] }
  | WITH acts=separated_nonempty_list(COMMA, action) { acts }

action:
  | i = LID  { mk ~loc:$sloc (Lang.L.Syntax.Emit (mk_ident i)) }
  | l = lhs COLEQ e = expr { mk ~loc:$sloc (Lang.L.Syntax.Assign (l, e)) }

(* IOS *)

global:
  | INPUT id=id COLON ty=type_expr EQUAL st=stimuli
      { [mk ~loc:$sloc (mk_global_ident id, Lang.L.Syntax.Input, ty, Some st)] }
  | OUTPUT ids=separated_nonempty_list(COMMA,id) COLON ty=type_expr
      { List.map (fun id -> mk ~loc:$sloc (mk_global_ident id, Lang.L.Syntax.Output, ty, None)) ids }
  | SHARED ids=separated_nonempty_list(COMMA,id) COLON ty=type_expr
      { List.map (fun id -> mk ~loc:$sloc (mk_global_ident id, Lang.L.Syntax.Shared, ty, None)) ids }

stimuli:
  | PERIODIC LPAREN p=INT COMMA s=INT COMMA d=INT RPAREN
      { mk ~loc:$sloc (Lang.L.Syntax.Periodic(p,s,d)) }
  | SPORADIC ts=paren(separated_list(COMMA,INT))
      { mk ~loc:$sloc (Lang.L.Syntax.Sporadic(ts)) }
  | VALUE_CHANGES vcs=paren(separated_list(COMMA,value_change))
      { mk ~loc:$sloc (Lang.L.Syntax.Value_change(vcs)) }
  
value_change:
  | t=INT COLON v=stim_const { (t,v) }
  
(* FSM INSTANCES *)

fsm_inst:
  | FSM name=id EQUAL model=id params=optional(inst_params) args=paren(separated_list(COMMA,id))
      { mk ~loc:$sloc (mk_global_ident name, mk_global_ident model, params, List.map mk_global_ident args) }  

inst_params:
  |  LT params=separated_nonempty_list(COMMA, param_value) GT { params }

id:
  | i = LID { i }
  | i = UID { i }

#include "guest_rules.mly"
