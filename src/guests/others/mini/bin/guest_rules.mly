/* Parser rules for the guest language */

type_decl:
  | TYPE /* Not used in this guest language */ { mk ~loc:($symbolstartofs,$endofs) () } 

param_value:
  | v = scalar_const { v } /* Not used in this guest language */ 

type_expr:
  | tc = LID { mk ~loc:($symbolstartofs,$endofs) (TeConstr tc) }

lhs:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (mk_ident v) }

expr:
  | v = LID { mk ~loc:($symbolstartofs,$endofs) (EVar (mk_ident v)) }
  | c = scalar_const { c }

scalar_const:
  | TRUE { mk ~loc:($symbolstartofs,$endofs) (EBool true) }
  | FALSE { mk ~loc:($symbolstartofs,$endofs) (EBool false) }

const:
  | c = scalar_const { c }

stim_const: 
  | c = scalar_const { c }

