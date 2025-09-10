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
  type fragment_obj
  exception Error
  val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> program
  val fragment_obj: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> fragment_obj
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
             and type fragment_obj = L.Syntax.fragment_obj)
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

  let check_fragment (f: Fragment.t) =
    let open L in
    try
      let mk_iov (id,t) = Ident.mk id, Syntax.mk_basic_type_expr t in
      let pf = {
        Syntax.pf_inps = List.map mk_iov f.jf_inps;
        Syntax.pf_outps = List.map mk_iov f.jf_outps;
        Syntax.pf_vars = List.map mk_iov f.jf_vars;
        Syntax.pf_obj = analyse_string ~lexer:Lexer.main ~parser:Parser.fragment_obj f.jf_obj
        } in
      Format.printf "rfsm server: parsed fragment = %a" L.Syntax.pp_fragment pf;
      L.Syntax.check_fragment pf; (* TBR ?  Redundant with type-checking ? *)
      let ppf = L.Syntax.ppr_fragment pf in
      Format.printf "rfsm server: pre-processed fragment = %a" L.Syntax.pp_fragment ppf;
      type_fragment ppf;
      Response.NoErr
     with exn ->
       Printf.printf "rfsm server: check_fragment raised %s\n" (Printexc.to_string exn); flush stdout;
       begin match exn with
         (* The exceptions listed here will _not_ reach [Error.handle] *)
         | L.Syntax.Invalid_symbol (id,_,reason) ->
             Response.SemanticErr (Printf.sprintf "symbol %s: %s" (Ident.to_string id) reason)
         | Typing.Type_mismatch(_,ty,ty') ->  
             Response.TypingErr (Printf.sprintf "expected type here was %s, not %s\n"
                                 ty
                                 (Ext.Format.to_string L.Typing.HostSyntax.pp_typ ty'))
         | L.Guest.Typing.Type_conflict (loc,ty,ty') -> 
             Response.TypingErr (Printf.sprintf "cannot unify types %s and %s"
                                 (Ext.Format.to_string L.Guest.Types.pp_typ ty)
                                 (Ext.Format.to_string L.Guest.Types.pp_typ ty'))
         | Parser.Error ->
             Response.SyntaxErr
         | _ ->
             Response.OtherErr (Printexc.to_string exn)

       end 

  let handle_request req =
    match req with
    | Request.GetVersion -> Response.Version Version.version
    | Request.CheckFragment pf -> check_fragment pf
    
  let service ic oc =
    try
      while true do
        Printf.printf "rfsm server: waiting for request\n" ; flush stdout;
        let line = input_line ic in
        Printf.printf "rfsm server: got request: %s\n" line ; flush stdout;
        let request = Request.from_string line in
        Format.printf "rfsm server: decoded request: %a\n" Request.pp request; flush stdout;
        let response = handle_request request in
        Format.printf "rfsm server: response = %a\n" Response.pp response; flush stdout;
        let line' = Response.to_string response in
        Printf.printf "rfsm server: encoded response: %s\n"  line'; flush stdout;
        output_string oc (line' ^ "\n");
        flush oc
      done
    with exn ->
      Printf.printf "rfsm server: caught exn %s. Ending\n" (Printexc.to_string exn); flush stdout ;
      exit 0

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
