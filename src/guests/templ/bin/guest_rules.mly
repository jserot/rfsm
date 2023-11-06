/* Parser rules for the guest language */
/* These rules will be added to the parser of the host language in file ../../../../src/host/lib/parser.cpp.mly */

/* AST nodes should be built using the [mk] function defined in ../lib/syntax.ml, which take an extra parameter
 * for storing the location of the corresponding concrete synatx fragment in the source code.
 * For ex (taken from ../../core/bin/guest_rules.mly) :
 * expr:
    | v = LID { mk ~loc:$sloc (EVar (mk_ident v)) } */

type_decl:
  /* TO BE COMPLETED */
  /* Define here the syntax for type declarations.

param_value:
  /* TO BE COMPLETED */
  /* Define here the syntax for instance parameter values.

type_expr:
  /* TO BE COMPLETED */
  /* Define here the syntax of type expressions */

expr:
  /* TO BE COMPLETED */
  /* Define here the syntax of expressions */

lhs:
  /* TO BE COMPLETED */
  /* Define here the syntax of LHS */

const:
  /* TO BE COMPLETED */
  /* Define here the syntax of constants */

scalar_const:
  /* TO BE COMPLETED */
  /* Define here the syntax of scalar constants */

stim_const: 
  /* TO BE COMPLETED */
  /* Define here the syntax of constants which can be used in the definition of input stimuli */

