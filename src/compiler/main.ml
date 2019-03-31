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
  Printf.printf "------------------------------------------------------------------\n";
  Printf.printf "Reactive Finite State Machine compiler and simulator, version %s\n" Version.version;
  Printf.printf "For information: github.com/jserot/rfsm\n"; 
  Printf.printf "------------------------------------------------------------------\n";
  flush stdout

let parse lexer parser fname = 
  let ic = open_in_bin fname in
  (* The source file(s) must be opened in binary mode, so that the absolute seeks in print_location work. *)
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
         ~fsm_options:(if !Options.dot_captions then [] else [Fsm.NoCaption])
         !Options.target_dir
         m;
  | Some Options.CTask ->
       Ctask.check_allowed m;
       check_dir !Options.target_dir;
       begin match m with
       | Static.Models ms ->
          List.iter (Ctask.dump_fsm_model ~dir:!Options.target_dir m) ms
       | Static.System s ->
          if Ctask.need_globals s then Ctask.dump_globals ~dir:!Options.target_dir s;
          List.iter (Ctask.dump_fsm_inst ~dir:!Options.target_dir s) s.Sysm.m_fsms
       end
  | Some Options.SystemC ->
       Systemc.check_allowed m;
       check_dir !Options.target_dir;
       begin match m with
       | Static.Models ms ->
          List.iter (Systemc.dump_fsm_model ~dir:!Options.target_dir) ms
       | Static.System s ->
          if Systemc.need_globals s then Systemc.dump_globals ~dir:!Options.target_dir s;
          Systemc.dump_model ~dir:!Options.target_dir s;
          Systemc.dump_testbench ~name:name ~dir:!Options.target_dir s;
          Systemc.dump_makefile ~name:name ~dir:!Options.target_dir s
       end
  | Some Options.Vhdl ->
       check_dir !Options.target_dir;
       begin match m with
       | Static.Models ms ->
          Vhdl.check_allowed_models ms;
          List.iter (Vhdl.dump_fsm_model ~dir:!Options.target_dir) ms
       | Static.System s ->
          Vhdl.check_allowed_system s;
          if Vhdl.need_globals s then Vhdl.dump_globals ~dir:!Options.target_dir s;
          Vhdl.dump_model ~dir:!Options.target_dir s;
          Vhdl.dump_testbench ~name:name ~dir:!Options.target_dir s;
          Vhdl.dump_makefile ~name:name ~dir:!Options.target_dir s
       end
  | Some Options.Sim ->
       begin match m with
       | Static.System s ->
          let ctx, reacts = Simul.run s in
          Vcd.output s ctx !Options.vcd_file reacts
       | _ ->
          eprintf "No testbench to simulate.\n"; flush stderr; exit 1
       end
  | None ->
     ()
  end;
  Logfile.stop ()
with
| e -> Error.handle e

let _ = Printexc.print main ()
