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

open Printf
open Location
   
let handle e = match e with
  | Old_parser.Error
  | Main_parser.Error ->
     let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
     let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
     eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
     flush stderr; exit 1
  | Old_lexer.Lexical_error(Old_lexer.Illegal_character, pos1, pos2)
  | Main_lexer.Lexical_error(Main_lexer.Illegal_character, pos1, pos2) ->
     eprintf "%aIllegal character.\n" output_location (Loc(!input_name,pos1, pos2)); flush stderr; exit 1
  | Types.Index.Illegal_type_index i -> 
     eprintf "Illegal type index: %s\n" (Types.Index.to_string i); flush stderr; exit 2
  | Types.Index.Unbound_type_index v -> 
     eprintf "Unbound type index: %s\n" v; flush stderr; exit 2
  | Types.Index.Illegal_op op -> 
     eprintf "Illegal operation on type index: %s\n" op; flush stderr; exit 2
  | Typing.Undef_symbol (where, what,id) -> 
     eprintf "Cannot retrieve type for %s \"%s\"\n" what id; flush stderr; exit 2
  | Typing.Unbound_type_ctor c -> 
     eprintf "Unbound type constructor: %s\n" c; flush stderr; exit 2
  | Builtins.Unbound_id id -> 
     eprintf "Unknown builtin operator: %s\n" id; flush stderr; exit 3
  | Eval.Unknown_id id -> 
     eprintf "Unknown identifier: %s\n" id; flush stderr; exit 3
  | Eval.Illegal_expr e -> 
     eprintf "Illegal expression: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Eval.Illegal_application e -> 
     eprintf "Illegal application: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Eval.Non_static_expr (e,e') -> 
     eprintf "The sub-expression \"%s\" in expression \"%s\" cannot be statically evaluated\n"
       (Expr.to_string e') (Expr.to_string e); flush stderr; exit 3
  | Typing.Illegal_cast e -> 
     eprintf "Illegal type cast: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Typing.Invalid_record_access e -> 
     eprintf "Illegal record access: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Elab.Unbound_fsm (loc,fsm) ->
     eprintf "%aNo definition for FSM %s.\n" output_location loc fsm; flush stderr; exit 3
  | Elab.Unbound_global (loc,id) ->
     eprintf "%aNo declaration for global value %s.\n" output_location loc id; flush stderr; exit 3
  | Elab.Fsm_mismatch (what,loc,fsm) ->
     eprintf "%aThe number of %s supplied for FSM %s does not match its declaration.\n" output_location loc what fsm;
     flush stderr; exit 3
  | Eval.Illegal_array_access e -> 
     eprintf "Illegal array access: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Eval.Illegal_bit_range_access e -> 
     eprintf "Illegal bit_range access: %s\n" (Expr.to_string e); flush stderr; exit 3
  | Eval.Invalid_array_access (a,i) -> 
     eprintf "Array access out of bound: %s[%d]\n" a i; flush stderr; exit 3
  | Fsm.Undef_symbol (fsm, what, id) ->
     eprintf "Undefined %s in FSM %s:  %s\n" what fsm id; flush stderr; exit 4
  | Fsm.Invalid_state (fsm, id) ->
     eprintf "Invalid state in FSM %s:  %s\n" fsm id; flush stderr; exit 4
  | Fsm.Binding_mismatch (fsm, what, "") ->
     eprintf "Error when binding %s for FSM %s\n" what fsm; flush stderr; exit 4
  | Fsm.Binding_mismatch (fsm, what, id) ->
     eprintf "Error when binding %s for FSM %s:  %s\n" what fsm id; flush stderr; exit 4
  | Fsm.Invalid_parameter (fsm, id) ->
     eprintf "Invalid parameter value for FSM %s:  %s\n" fsm id; flush stderr; exit 4
  | Typing.Typing_error (what, where, ty, ty') ->
     eprintf "Error when typing %s in %s: types %s and %s are not compatible\n"
       what where (Types.string_of_type ty) (Types.string_of_type ty');
     flush stderr; exit 4
  | Dynamic.NonDetTrans (m,ts,t) ->
      eprintf "Error when simulating FSM %s: non deterministic transitions found at t=%d:\n" m.Dynamic.f_static.Fsm.f_name t;
      List.iter (function t -> eprintf "\t- %s\n" (Fsm.string_of_transition t)) ts;
      flush stderr; exit 7
  | Dynamic.IllegalTrans (m,msg) ->
      eprintf "Error when simulating FSM %s: %s\n" m.Dynamic.f_static.Fsm.f_name msg; flush stderr; exit 7
  | Dynamic.Undeterminate (m,id,t) ->
      eprintf "Error when simulating FSM %s: unknown value for identifier %s at t=%d\n" m.Dynamic.f_static.Fsm.f_name id t;
      flush stderr; exit 7
  | Dynamic.NonAtomicIoWrite(m,a) ->
     eprintf "Illegal action \"%s\" within FSM \"%s\": non atomic write(s) are forbidden for global IOs and shared objects\n"
       (Action.to_string a) m.Dynamic.f_static.Fsm.f_name;
  | Dynamic.IllegalAction(m,a) ->
     eprintf "Error when executing action \"%s\" within FSM \"%s\" (check for undefined values...)\n"
       (Action.to_string a) m.Dynamic.f_static.Fsm.f_name;
     flush stderr; exit 7
  (* | Intern.Incomplete_record (where,v,ty) ->
   *    eprintf "Found incomplete record value in %s: value %s does not have (full) type %s\n"
   *      where (Expr.string_of_value v) (Types.string_of_type ty);
   *    flush stderr; exit 7 *)
  | Simul.OverReaction t ->
      eprintf "Simulation loops (over-reaction) at t=%d\n" t; flush stderr; exit 5
  | Cmodel.Error (m,msg) ->
     eprintf "Error when translating FSM <%s> to C model: %s\n" m msg; flush stderr; exit 6
  | Systemc.Error (where,msg) ->
     eprintf "Error when generating SystemC code (%s): %s\n" where msg; flush stderr; exit 7
  | Vhdl.Error ("",msg) ->
     eprintf "Error when generating VDHL: %s\n" msg; flush stderr; exit 8
  | Vhdl.Error (m,msg) ->
     eprintf "Error when generating VDHL for FSM %s: %s\n" m msg; flush stderr; exit 8
  | Vcd.Error msg ->
      eprintf "Error when generating VCD file: %s\n" msg; flush stderr; exit 9
  | Misc.Not_implemented msg ->
     eprintf "Not implemented: %s.\n" msg; flush stderr; exit 22
  | Misc.Internal_error msg ->
     eprintf "Internal error: %s.\n" msg; flush stderr; exit 23
  | Sys_error msg ->
     eprintf "Input/output error: %s.\n" msg; flush stderr; exit 21
  | Sys.Break -> flush stderr; exit 20
  | End_of_file -> exit 0
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100

(* let io_mismatch loc what id =
 *   eprintf "%aNo global declaration for %s %s.\n" 
 *     output_location loc
 *     what
 *     id;
 *   raise Model_error
 * 
 * let unbound_expr_index id expr  = 
 *   eprintf "The identifier %s is unbound in index expression:  %s.\n" 
 *   id
 *   (Type_expr.string_of_type_index expr);
 *   raise Model_error
 * 
 * let illegal_expr_index id expr  = 
 *   eprintf "Illegal type index expression:  %s.\n" 
 *   (Type_expr.string_of_type_index expr);
 *   raise Model_error *)
