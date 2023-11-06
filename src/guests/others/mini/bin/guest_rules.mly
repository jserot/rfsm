/* Parser rules for the guest language */

type_decl:
  | TYPE /* Not used in this guest language */ { mk ~loc:$sloc () } 

param_value:
  | v = scalar_const { v } /* Not used in this guest language */ 

type_expr:
  | tc = LID { mk ~loc:$sloc (TeConstr tc) }

lhs:
  | v = LID { mk ~loc:$sloc (mk_ident v) }

expr:
  | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) }
  | c = scalar_const { c }

scalar_const:
  | TRUE { mk ~loc:$sloc (EBool true) }
  | FALSE { mk ~loc:$sloc (EBool false) }

const:
  | c = scalar_const { c }

stim_const: 
  | c = scalar_const { c }

