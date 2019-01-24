(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

exception Model_error
exception Not_implemented of string

open Printf
open Location
open Syntax
   
let fatal_error msg = raise (Misc.Internal_error msg)

let not_implemented msg = raise (Not_implemented msg)

let warning msg = Printf.printf "** Warning: %s\n" msg

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

let unbound_expr_index id expr  = 
  eprintf "The identifier %s is unbound in index expression:  %s.\n" 
  id
  (Type_expr.string_of_type_index expr);
  raise Model_error

let illegal_expr_index id expr  = 
  eprintf "Illegal type index expression:  %s.\n" 
  (Type_expr.string_of_type_index expr);
  raise Model_error
