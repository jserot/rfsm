(* The parser for the guest language *)
(* See file ../../../../host/lib/host_parser.mly for the list of keywords already defined by the host language *)

%token TRUE
%token FALSE

%{
open Mini.Top.Syntax
%}

%%
%public type_decl:
  | TYPE /* Not used in this guest language */ { mk ~loc:$sloc () } 

%public param_value:
  | v = scalar_const { v } /* Not used in this guest language */ 

%public type_expr:
  | tc = LID { mk ~loc:$sloc (TeConstr tc) }

%public lval:
  | v = LID { mk ~loc:$sloc (mk_ident v) }

%public expr:
  | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) }
  | c = scalar_const { c }

%public scalar_const:
  | TRUE { mk ~loc:$sloc (EBool true) }
  | FALSE { mk ~loc:$sloc (EBool false) }

%public const:
  | c = scalar_const { c }

%public stim_const: 
  | c = scalar_const { c }

