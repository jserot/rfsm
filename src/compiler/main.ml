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

let source_files = ref ([] : string list)

let anonymous fname = source_files := !source_files @ [fname]

let print_banner () = 
  Printf.printf "-------------------------------------------------------------------------------------------------\n";
  Printf.printf "Reactive Finite State Machine compiler and simulator, version %s\n" Version.version;
  Printf.printf "http://github.com/jserot/rfsm\n"; 
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

let main () =
try
  Sys.catch_break true;
  Arg.parse Options_spec.options_spec anonymous usage;
  print_banner ();
  if !Options.print_version then exit 0;
  let name =
    begin match !Options.main_name, !source_files with
    | "", [] -> eprintf "No input file(s)\n"; flush stderr; exit 1
    | "",  srcs -> Filename.chop_extension (Filename.basename (List.hd (List.rev srcs)))
    | n, _ -> n
    end in
  let p =
    List.fold_left
      (fun p f -> Syntax.add_program p (parse Main_lexer.main Main_parser.program f))
      Syntax.empty_program
      !source_files in
  let m = Static.elaborate name p in
  if !Options.dump_static then Static.dump stdout m;
  Logfile.start ();
  begin match !Options.target with
  | Some Options.Dot ->
       check_dir !Options.target_dir;
       Static.dot_output
         ~fsm_options:(if !Options.dot_captions then [] else [Fsm.Static.NoCaption])
         ~with_insts:!Options.dot_fsm_insts
         ~with_models:!Options.dot_fsm_models
         !Options.target_dir
         m;
  | Some Options.CTask ->
       Ctask.check_allowed m;
       check_dir !Options.target_dir;
       if Ctask.need_globals m then Ctask.dump_globals ~dir:!Options.target_dir m;
       List.iter (Ctask.dump_fsm ~dir:!Options.target_dir m) m.Sysm.m_fsms
  | Some Options.SystemC ->
       Systemc.check_allowed m;
       check_dir !Options.target_dir;
       if Systemc.need_globals m then Systemc.dump_globals ~dir:!Options.target_dir m;
       Systemc.dump_model ~dir:!Options.target_dir m;
       Systemc.dump_testbench ~dir:!Options.target_dir m;
       Systemc.dump_makefile ~dir:!Options.target_dir m
  | Some Options.Vhdl ->
       Vhdl.check_allowed m;
       check_dir !Options.target_dir;
       if Vhdl.need_globals m then Vhdl.dump_globals ~dir:!Options.target_dir m;
       Vhdl.dump_model ~dir:!Options.target_dir m;
       Vhdl.dump_testbench ~dir:!Options.target_dir m;
       Vhdl.dump_makefile ~dir:!Options.target_dir m
  (* | Some Options.Sim ->
   *      let ctx, reacts = Simul.run m in
   *      Vcd.output m ctx !Options.vcd_file reacts *)
  | None ->
     ()
  end;
  Logfile.stop ()
with
| e -> Error.handle e

    
let _ = Printexc.print main ()
