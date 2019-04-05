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

let check_dir path = 
  if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory"))

let print_banner () = 
  Printf.printf "------------------------------------------------------------------\n";
  Printf.printf "Reactive Finite State Machine compiler and simulator, version %s\n" Version.version;
  Printf.printf "For information: github.com/jserot/rfsm\n"; 
  Printf.printf "------------------------------------------------------------------\n";
  flush stdout

let parse fname = 
  let ic = open_in_bin fname in
  (* The source file(s) must be opened in binary mode, so that the absolute seeks in print_location work. *)
  Location.input_name := fname;
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel !Location.input_chan in
  Location.input_lexbuf := lexbuf;
  if !Options.use_old_syntax then 
    Old_parser.program Old_lexer.main !Location.input_lexbuf
  else
    Main_parser.program Main_lexer.main !Location.input_lexbuf 

let rewrite_syntax fname =
  let p = parse fname in
  let fname' = Utils.FilenameExt.add_before_suffix fname ".new" in
  Printf.printf "** Translating %s... " fname; flush stdout;
  let oc = open_out fname' in
  Syntax.dump_program oc Syntax.Syntax_new p;
  close_out oc;
  Printf.printf "Done (translated source is in file %s)\n" fname'; flush stdout

let compile name =
  let p =
    List.fold_left
      (fun p f -> Syntax.add_program p (parse f))
      Syntax.empty_program
      !source_files in
  let s, has_testbench = Elab.process name p in
  if !Options.dump_static then Static.dump stdout s;
  Logfile.start ();
  begin match !Options.target with
  | Some Options.Dot ->
     check_dir !Options.target_dir;
     let fs = Static.dot_output
       ~fsm_options:(if !Options.dot_captions then [] else [Fsm.NoCaption])
       !Options.target_dir
       s in
     List.iter Logfile.write fs
  | Some Options.CTask ->
     Ctask.check_allowed s;
     check_dir !Options.target_dir;
     if Ctask.need_globals s then Ctask.dump_globals ~dir:!Options.target_dir s;
     List.iter (Ctask.dump_fsm_model ~dir:!Options.target_dir s) s.Static.m_models
  | Some Options.SystemC ->
     Systemc.check_allowed s;
     check_dir !Options.target_dir;
     if Systemc.need_globals s then Systemc.dump_globals ~dir:!Options.target_dir s;
     if has_testbench then begin
         List.iter (Systemc.dump_input ~dir:!Options.target_dir s) s.Static.m_inputs;
         List.iter (Systemc.dump_fsm_inst ~dir:!Options.target_dir s) s.Static.m_fsms;
         Systemc.dump_testbench ~name:name ~dir:!Options.target_dir s;
         Systemc.dump_makefile ~name:name ~dir:!Options.target_dir s
       end
     else
         List.iter (Systemc.dump_fsm_model ~dir:!Options.target_dir) s.Static.m_models
  | Some Options.Vhdl ->
     check_dir !Options.target_dir;
     Vhdl.check_allowed_system s;
     if Vhdl.need_globals s then Vhdl.dump_globals ~dir:!Options.target_dir s;
     if has_testbench then begin
         List.iter (Vhdl.dump_fsm_inst ~dir:!Options.target_dir s) s.Static.m_fsms;
         Vhdl.dump_toplevel ~name:name ~dir:!Options.target_dir s;
         Vhdl.dump_testbench ~name:name ~dir:!Options.target_dir s;
         Vhdl.dump_makefile ~name:name ~dir:!Options.target_dir s
       end
     else
         List.iter (Vhdl.dump_fsm_model ~dir:!Options.target_dir) s.Static.m_models
  | Some Options.Sim ->
     if has_testbench then
        let fname = !Options.target_dir ^ "/" ^ !Options.main_prefix ^ ".vcd" in
        let ctx, reacts = Simul.run s in
        Vcd.output s ctx fname reacts
     else begin
         eprintf "No testbench to simulate.\n"; flush stderr;
         exit 1
       end
  | None ->
     ()
  end;
  Logfile.stop ()

let main () =
try
  Sys.catch_break true;
  Arg.parse Options_spec.options_spec anonymous usage;
  print_banner ();
  if !Options.print_version then exit 0;
  if !Options.transl_syntax then 
    List.iter rewrite_syntax !source_files
  else
    compile !Options.main_prefix
with
| e -> Error.handle e

let _ = Printexc.print main ()
