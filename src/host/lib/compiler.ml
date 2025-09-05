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

(**{1 The command-line compiler} *)
    
(** Output signature of the functor {!Compiler.Make} *)
module type T = sig
  val main: unit -> unit
end

(** Signature for the [Parser] input to the functor {!Compiler.Make} *)
module type PARSER = sig
  type token 
  type program
  type fragment
  exception Error
  val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> program
  val fragment: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> fragment
end

(** Signature for the [Lexer] input to the functor {!Compiler.Make} *)
module type LEXER = sig
  type token
  type lexical_error = Illegal_character
  exception Lexical_error of lexical_error * int * int
  val main: Lexing.lexbuf -> token
end

(** Functor building the compiler structure given a language definition, a lexer and a parser implementation *)
module Make
         (L: Host.T)
         (Lexer: LEXER)
         (Parser: PARSER
            with type token = Lexer.token
             and type program = L.Syntax.program
             and type fragment = L.Syntax.fragment)
  : T =
struct

  module Error = Error.Make(L)(Lexer)(Parser)
      
  let usage = "usage: rfsmc [options...] files"

  let source_files = ref ([] : string list)

  let anonymous fname = source_files := !source_files @ [fname]

  let print_banner () = 
    Printf.printf "---------------------------------------------------------------------------\n";
    Printf.printf "Reactive Finite State Machine compiler and simulator, version %s/%s-%s\n"
      Version.version L.Guest.Info.name L.Guest.Info.version ;
    Printf.printf "For information: github.com/jserot/rfsm\n"; 
    Printf.printf "---------------------------------------------------------------------------\n";
    flush stdout

  let analyse_file ~lexer:lexer ~parser:parse fname = 
    let ic = open_in_bin fname in
    Location.input_name := fname;
    Location.input_chan := ic;
    let lexbuf = Lexing.from_channel !Location.input_chan in
    Location.input_lexbuf := lexbuf;
    parse lexer !Location.input_lexbuf

  let analyse_string ~lexer:lexer ~parser:parse s = 
    let lexbuf = Lexing.from_string s in
    Location.input_lexbuf := lexbuf;
    parse lexer !Location.input_lexbuf

  let compile f =
    let open L in
    let p0 =
      List.fold_left
        (fun p f -> 
           let p' = analyse_file ~lexer:Lexer.main ~parser:Parser.program f in
           Syntax.add_program p p')
        Syntax.empty_program
        !source_files in
    let p = Syntax.ppr_program p0 in
    (* Format.printf "parsed=%a" pp_program p; *)
    let tenv0 = Typing.mk_env () in
    if !Options.dump_tenv then Format.printf "tenv=%a@." pp_tenv tenv0;
    let tp = type_program tenv0 p in
    if !Options.dump_typed then Format.printf "tp=%a@." Typing.pp_typed_program tp;
    let s = elab tp p in
    if !Options.dump_static then Format.printf "s=%a@." (Static.pp ~verbose_level:2) s;
    Logfile.start ();
    begin match !Options.target with
    | Some Options.Dot ->
       Ext.File.check_dir !Options.target_dir;
       let fs = Dot.output_static ~dir:!Options.target_dir ~name:!Options.main_prefix s in
       List.iter Logfile.write fs
    | Some Options.CTask ->
       Ext.File.check_dir !Options.target_dir;
       let fs = Ctask.output ~dir:!Options.target_dir s in
       List.iter Logfile.write fs
    | Some Options.SystemC ->
       Ext.File.check_dir !Options.target_dir;
       let fs = Systemc.output ~dir:!Options.target_dir ~pfx:!Options.main_prefix s in
       List.iter Logfile.write fs
    | Some Options.Vhdl ->
       Ext.File.check_dir !Options.target_dir;
       let fs = Vhdl.output ~dir:!Options.target_dir ~pfx:!Options.main_prefix s in
       List.iter Logfile.write fs
    | Some Options.Sim ->
       if s.fsms <> [] then
         let vcd_file = !Options.target_dir ^ "/" ^ !Options.main_prefix ^ ".vcd" in
         run ~vcd_file p s
       else begin
           Printf.eprintf "No testbench to simulate.\n"; flush stderr;
           exit 1
         end
    | None ->
       ()
    end;
    Logfile.stop ()

  (* let check_fragment () =
   *   let open L in
   *   match !source_files with
   *   | [f] ->
   *     let tenv0 = Typing.mk_env () in
   *     if !Options.dump_tenv then Format.printf "tenv=%a@." pp_tenv tenv0;
   *     let p = analyse ~lexer:Lexer.main ~parser:Parser.fragment f in
   *     (\* Format.printf "parsed=%a" L.Syntax.pp_fragment p; *\)
   *     L.Syntax.check_fragment p;
   *     p |> L.Syntax.ppr_fragment
   *       |> type_fragment tenv0
   *   | _ ->
   *     Format.eprintf "Usage: rfsmc -check_fragment [options] file\n";
   *     flush stderr;
   *     exit 1 *)

  let check_fragment f =
      (* let open L in *)
      (* let tenv0 = Typing.mk_env () in *)
    try
      let p = analyse_string ~lexer:Lexer.main ~parser:Parser.fragment f in
      let answer = Ext.Format.to_string L.Syntax.pp_fragment p in
      answer
     with exn ->
      Printf.printf "check_fragment raised %s\n" (Printexc.to_string exn); flush stdout ;
      "<failed>"

  let service ic oc =
    try
      while true do
        Printf.printf "rfsm server: waiting for request\n" ; flush stdout;
        let fragment = input_line ic in
        Printf.printf "rfsm server: got fragment: %s\n" fragment ; flush stdout;
        if not (String.starts_with ~prefix:"quit" fragment) then
          let answer = String.escaped (check_fragment fragment) in
          Printf.printf "rfsm server: sending response: %s\n" answer ; flush stdout;
          output_string oc (answer^"\n") ;
          flush oc
      done
    with exn ->
      Printf.printf "rfsm server: caught exn %s. Ending\n" (Printexc.to_string exn); flush stdout ;
      exit 0

  (* let service ic oc =
   *   try
   *     while true do
   *       Printf.printf "rfsm server: waiting for request\n" ; flush stdout;
   *       let s = input_line ic in
   *       Printf.printf "rfsm server: got: %s\n" s ; flush stdout;
   *       if s <> "QUIT" then
   *         let r = String.uppercase_ascii s
   *         in output_string oc (r^"\n") ; flush oc
   *     done
   *   with exn ->
   *     Printf.printf "rfsm server: caught exn %s. Ending\n" (Printexc.to_string exn); flush stdout ;
   *     exit 0 *)

  let main () =
    try
      Sys.catch_break true;
      Printexc.record_backtrace !Options.dump_backtrace;
      Arg.parse (Options.spec @ L.Guest.Options.specs) anonymous usage;
      if !Options.server_mode then
        Server.start ~socket:!Options.socket_path ~fn:service
      else
        begin
          print_banner ();
          compile !Options.main_prefix
      end
    with
      e -> Error.handle e

end
