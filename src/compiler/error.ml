exception Model_error
exception Internal_error of string
exception Not_implemented of string

open Printf
open Location
open Syntax
   
let fatal_error msg = raise (Internal_error msg)

let not_implemented msg = raise (Not_implemented msg)

let warning msg = Printf.printf "** Warning: %s\n" msg

(* let invalid_param_value loc p v =
 *   eprintf "%aInvalid value (%s) for parameter %s.\n"
 *     output_location loc
 *     (Expr.string_of_value v)
 *     p;
 *   raise Model_error *)

let unbound_fsm loc fsm = 
  eprintf "%aNo definition for FSM %s.\n" output_location loc fsm;
  raise Model_error

let unbound_global loc id = 
  eprintf "%aNo declaration for global value %s.\n" output_location loc id;
  raise Model_error

let fsm_mismatch what loc fsm = 
  eprintf "%aThe number of %s supplied for FSM %s does not match its declaration.\n" 
    output_location loc
    what
    fsm;
  raise Model_error

let io_mismatch loc what id =
  eprintf "%aNo global declaration for %s %s.\n" 
    output_location loc
    what
    id;
  raise Model_error

(* let type_mismatch loc what id ty ty' = 
 *   eprintf "%aThe types %s and %s for %s %s are not compatible.\n" 
 *     output_location loc
 *     ty
 *     ty'
 *     what
 *     id;
 *   raise Model_error *)

let unbound_expr_index id expr  = 
  eprintf "The identifier %s is unbound in index expression:  %s.\n" 
  id
  (Syntax.string_of_type_index expr);
  raise Model_error

let illegal_expr_index id expr  = 
  eprintf "Illegal type index expression:  %s.\n" 
  (Syntax.string_of_type_index expr);
  raise Model_error

(* let invalid_state where id = 
 *   eprintf "State %s used in definition of FSM %s has not been declared.\n" id where;
 *   raise Model_error *)

(* let undecl_fsm_sym where what id =
 *   eprintf "The symbol %s, occuring in transitions of FSM %s is not declared as %s.\n" id where what;
 *   raise Model_error *)
