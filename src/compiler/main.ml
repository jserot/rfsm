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

let usage = "usage: rfsmc [options...] files"

let source_file = ref ""

let anonymous fname = source_file := fname

let print_banner () = 
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  Printf.printf "Reactive Finite State Machine compiler and simulator, version %s\n" Version.version;
  Printf.printf "(C) 2018 J. Serot (jocelyn.serot@uca.fr)\n";
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  flush stdout

let parse lexer parser fname = 
  let ic = open_in_bin fname in
  (* The source file must be opened in binary mode, so that the absolute seeks in print_location work. *)
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  parser lexer !Location.input_lexbuf

let check_dir path = 
  if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory"))

exception Unbound_fsm of string

let main () =
try
  Sys.catch_break true;
  Arg.parse Options_spec.options_spec anonymous usage;
  print_banner ();
  if !Options.print_version then exit 0;
  let p = parse Main_lexer.main Main_parser.program !source_file in
  let name = Filename.chop_extension (Filename.basename !source_file) in
  let m = Intern.build_system name p in
  if !Options.dump_model then Sysm.dump stdout m;
  Logfile.start ();
  begin match !Options.target with
  | Some Options.Dot ->
       check_dir !Options.target_dir;
       Sysm.dot_output
         ~fsm_options:(if !Options.dot_captions then [] else [Fsm.NoCaption])
         ~with_insts:!Options.dot_fsm_insts
         ~with_models:!Options.dot_fsm_models
         !Options.target_dir
         m;
  | Some Options.CTask ->
       Ctask.check_allowed m;
       check_dir !Options.target_dir;
       List.iter (Ctask.dump_fsm ~dir:!Options.target_dir m) m.Sysm.m_fsms
  | Some Options.SystemC ->
       Systemc.check_allowed m;
       check_dir !Options.target_dir;
       Systemc.dump_model ~dir:!Options.target_dir m;
       Systemc.dump_testbench ~dir:!Options.target_dir m;
       Systemc.dump_makefile ~dir:!Options.target_dir m
  | Some Options.Vhdl ->
       Vhdl.check_allowed m;
       check_dir !Options.target_dir;
       Vhdl.dump_model ~dir:!Options.target_dir m;
       Vhdl.dump_testbench ~dir:!Options.target_dir m;
       Vhdl.dump_makefile ~dir:!Options.target_dir m
  | Some Options.Sim ->
       let ctx, reacts = Simul.run m in
       Vcd.output m ctx !Options.vcd_file reacts
  | None ->
     ()
  end;
  Logfile.stop ()
with
| Main_parser.Error ->
    let pos1 = Lexing.lexeme_start !Location.input_lexbuf in
    let pos2 = Lexing.lexeme_end !Location.input_lexbuf in
    eprintf "%aSyntax error\n" output_location (Loc(!input_name,pos1, pos2));
    flush stderr; exit 1
| Main_lexer.Lexical_error(Main_lexer.Illegal_character, pos1, pos2) ->
    eprintf "%aIllegal character.\n" output_location (Loc(!input_name,pos1, pos2)); flush stderr; exit 1
| Types.Index.Illegal_type_index i -> 
    eprintf "Illegal type index: %s\n" (Types.Index.to_string i); flush stderr; exit 2
| Types.Index.Unbound_type_index v -> 
    eprintf "Unbound type index: %s\n" v; flush stderr; exit 2
| Types.Index.Illegal_op op -> 
    eprintf "Illegal operation on type index: %s\n" op; flush stderr; exit 2
| Intern.Unbound_type_ctor c -> 
    eprintf "Unbound type constructor: %s\n" c; flush stderr; exit 2
| Builtins.Unbound_id id -> 
    eprintf "Unknown builtin operator: %s\n" id; flush stderr; exit 3
| Eval.Unknown_id id -> 
    eprintf "Unknown identifier: %s\n" id; flush stderr; exit 3
| Eval.Unbound_id id -> 
    eprintf "No value attached to identifier: %s\n" id; flush stderr; exit 3
| Eval.Illegal_expr e -> 
    eprintf "Illegal expression: %s\n" (Expr.to_string e); flush stderr; exit 3
| Fsm.Undef_symbol (fsm, what, id) ->
    eprintf "Undefined %s in FSM %s:  %s\n" what fsm id; flush stderr; exit 4
| Fsm.Invalid_state (fsm, id) ->
    eprintf "Invalid state in FSM %s:  %s\n" fsm id; flush stderr; exit 4
| Fsm.Binding_mismatch (fsm, what, "") ->
    eprintf "Error when binding %s for FSM %s\n" what fsm; flush stderr; exit 4
| Fsm.Binding_mismatch (fsm, what, id) ->
    eprintf "Error when binding %s for FSM %s:  %s\n" what fsm id; flush stderr; exit 4
| Fsm.Invalid_parameter (fsm, id) ->
    eprintf "Invalid parameter for FSM %s:  %s\n" fsm id; flush stderr; exit 4
| Fsm.Type_mismatch (fsm, what, where, ty, ty') ->
   eprintf "Error when typing %s %s in FSM %s: types %s and %s are not compatible\n"
     what where fsm (Types.string_of_type ty) (Types.string_of_type ty');
   flush stderr; exit 4
| Fsm.Type_error (fsm, what, item, ty, ty') ->
   eprintf "Error when typing %s \"%s\" for FSM %s: types %s and %s are not compatible\n"
     what item fsm (Types.string_of_type ty) (Types.string_of_type ty');
   flush stderr; exit 4
| Fsm.NonDetTrans (m,ts,t) ->
    eprintf "Error when simulating FSM %s: non deterministic transitions found at t=%d:\n" m.Fsm.f_name t;
    List.iter (function t -> eprintf "\t- %s\n" (Fsm.string_of_transition t)) ts;
    flush stderr; exit 7
| Fsm.IllegalTrans (m,msg) ->
    eprintf "Error when simulating FSM %s: %s\n" m.Fsm.f_name msg; flush stderr; exit 7
| Fsm.Undeterminate (m,id,t) ->
    eprintf "Error when simulating FSM %s: unknown value for identifier %s at t=%d\n" m.Fsm.f_name id t;
    flush stderr; exit 7
| Simul.OverReaction t ->
    eprintf "Simulation loops (over-reaction) at t=%d\n" t; flush stderr; exit 5
| Cmodel.Error (m,msg) ->
    eprintf "Error when translating FSM <%s> to C model: %s\n" m.Fsm.f_name msg; flush stderr; exit 6
| Systemc.Error (where,msg) ->
    eprintf "Error when generating SystemC code (%s): %s\n" where msg; flush stderr; exit 7
| Vhdl.Vhdl_error ("",msg) ->
    eprintf "Error when generating VDHL: %s\n" msg; flush stderr; exit 8
| Vhdl.Vhdl_error (m,msg) ->
    eprintf "Error when generating VDHL for FSM %s: %s\n" m msg; flush stderr; exit 8
| Vcd.Error msg ->
    eprintf "Error when generating VCD file: %s\n" msg; flush stderr; exit 9
| Sys.Break -> flush stderr; exit 20
| Sys_error msg ->
    eprintf "Input/output error: %s.\n" msg; flush stderr; exit 21
| Error.Not_implemented msg ->
    eprintf "Not implemented: %s.\n" msg; flush stderr; exit 22
| Error.Model_error ->
  ()
| Error.Internal_error msg
| Fsm.Internal_error msg ->
    eprintf "Internal error: %s.\n" msg; flush stderr; exit 23
| End_of_file -> exit 0
| e ->
    eprintf "Internal error: %s.\n" (Printexc.to_string e);
    flush stderr; exit 100
    
let _ = Printexc.print main ()
