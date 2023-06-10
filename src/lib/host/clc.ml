(* For building the command-line compiler *)

module type T = sig
  val main: unit -> unit
end

module type PARSER = sig
  type token 
  type program
  exception Error
  val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> program
end

module type LEXER = sig
  type token
  type lexical_error = Illegal_character
  exception Lexical_error of lexical_error * int * int
  val main: Lexing.lexbuf -> token
end

module Make
         (L: Host.T)
         (Lexer: LEXER)
         (Parser: PARSER with type token = Lexer.token and type program = L.Syntax.program) : T =
struct

  let usage = "usage: rfsmcc [options...] files"

  let source_files = ref ([] : string list)

  let anonymous fname = source_files := !source_files @ [fname]

  let print_banner () = 
    Printf.printf "---------------------------------------------------------------------------\n";
    Printf.printf "Reactive Finite State Machine compiler and simulator, version %s(%s)\n" Version.version L.Guest.Info.version;
    Printf.printf "For information: github.com/jserot/rfsm\n"; 
    Printf.printf "---------------------------------------------------------------------------\n";
    flush stdout

  let parse fname = 
    let ic = open_in_bin fname in
    Location.input_name := fname;
    Location.input_chan := ic;
    let lexbuf = Lexing.from_channel !Location.input_chan in
    Location.input_lexbuf := lexbuf;
    Parser.program Lexer.main !Location.input_lexbuf

  let compile f =
    let p =
      List.fold_left
        (fun p f -> L.Syntax.add_program p (parse f))
        L.Syntax.empty_program
        !source_files in
    (* if !Options.dump_parsed then Format.printf "parsed=%a" L.pp_program p; *)
    let tenv0 = L.Typing.mk_env () in
    if !Options.dump_tenv then Format.printf "tenv=%a" L.pp_tenv tenv0;
    L.type_program tenv0 p;
    if !Options.dump_typed then Format.printf "tp=%a" L.pp_program p;
    let s = L.elab p in
    (* let s = if !Options.normalize then Static.normalize s' else s' in *) (* TODO *)
    if !Options.dump_static then Format.printf "s=%a" (L.Static.pp ~verbose_level:2) s;
    Logfile.start ();
    begin match !Options.target with
    | Some Options.Dot ->
       Misc.check_dir !Options.target_dir;
       let fs =
         L.Dot.output_static 
           ~dir:!Options.target_dir
           ~name:!Options.main_prefix
           ~with_models:!Options.dot_show_models
           ~with_caption:!Options.dot_captions
           s in
       List.iter Logfile.write fs
    | Some Options.CTask ->
       Misc.not_implemented "CTask backend"
       (* Ctask.check_allowed s;
        * Misc.check_dir !Options.target_dir;
        * if Ctask.need_globals s then Ctask.dump_globals ~dir:!Options.target_dir s;
        * List.iter (Ctask.dump_fsm_model ~dir:!Options.target_dir s) s.Static.m_models *)
    | Some Options.SystemC ->
       Misc.not_implemented "Systemc backend"
       (* Systemc.check_allowed s;
        * Misc.check_dir !Options.target_dir;
        * if Systemc.need_globals s then Systemc.dump_globals ~dir:!Options.target_dir s;
        * if has_testbench then begin
        *     List.iter (Systemc.dump_input ~dir:!Options.target_dir s) s.Static.m_inputs;
        *     List.iter (Systemc.dump_fsm_inst ~dir:!Options.target_dir s) s.Static.m_fsms;
        *     Systemc.dump_testbench ~name:name ~dir:!Options.target_dir s;
        *     Systemc.dump_makefile ~name:name ~dir:!Options.target_dir s
        *   end
        * else
        *   List.iter (Systemc.dump_fsm_model ~dir:!Options.target_dir) s.Static.m_models *)
    | Some Options.Vhdl ->
       Misc.not_implemented "VHDL backend"
       (* Misc.check_dir !Options.target_dir;
        * Vhdl.check_allowed_system s;
        * if Vhdl.need_globals s then Vhdl.dump_globals ~dir:!Options.target_dir s;
        * if has_testbench then begin
        *     List.iter (Vhdl.dump_fsm_inst ~dir:!Options.target_dir s) s.Static.m_fsms;
        *     Vhdl.dump_toplevel ~name:name ~dir:!Options.target_dir s;
        *     Vhdl.dump_testbench ~name:name ~dir:!Options.target_dir s;
        *     Vhdl.dump_makefile ~name:name ~dir:!Options.target_dir s
        *   end
        * else
        *   List.iter (Vhdl.dump_fsm_model ~dir:!Options.target_dir) s.Static.m_models *)
    | Some Options.Sim ->
       if s.fsms <> [] then
         let vcd_file = !Options.target_dir ^ "/" ^ !Options.main_prefix ^ ".vcd" in
         L.run ~verbose_level:!Options.sim_trace_level ~vcd_file p s
       else begin
           Printf.eprintf "No testbench to simulate.\n"; flush stderr;
           exit 1
         end
    | None ->
       ()
    end;
    Logfile.stop ()

  let main () =
    let open Format in
    let open Location in
    try
      Sys.catch_break true;
      Printexc.record_backtrace !Options.dump_backtrace;
      Arg.parse (Options.spec @ L.Guest.Options.specs) anonymous usage;
      print_banner ();
      compile !Options.main_prefix
    with
    | Parser.Error ->
       let pos1 = Lexing.lexeme_start !input_lexbuf in
       let pos2 = Lexing.lexeme_end !input_lexbuf in
       eprintf "%aSyntax error\n" pp_location (Loc(!input_name,pos1, pos2));
       flush stderr; exit 1
    | Lexer.Lexical_error(Lexer.Illegal_character, pos1, pos2) ->
       eprintf "%aIllegal character.\n" pp_location (Loc(!input_name,pos1, pos2)); flush stderr; exit 1
    | L.Typing.Undefined_symbol (loc,s) -> 
       eprintf "%aUndefined symbol: %s\n" pp_location loc s; exit 2
    | L.Typing.Duplicate_state (loc,name) ->
       eprintf "%aDuplicate state name: %s\n" pp_location loc name; exit 2
    | L.Typing.Invalid_state (loc,name) ->
       eprintf "%aNo state named %s\n" pp_location loc name; exit 2
    | L.Typing.Illegal_inst loc ->
       eprintf "%aCannot instantiate model: formal and actual parameters do not match\n" pp_location loc; exit 2
    | L.Typing.No_event_input loc ->
       eprintf "%aThere must be at least one input with type event for this model\n" pp_location loc; exit 2
    | L.Typing.Illegal_state_output (loc,q,o) ->
       eprintf "%aIllegal valuation for output %s in state %s\n" pp_location loc o q; exit 2
    | L.Dynamic.Illegal_stimulus_value loc ->
       eprintf "%aIllegal stimulus value\n" pp_location loc; exit 2
    | L.Dynamic.Non_deterministic_transition (f, t, ts) ->
       eprintf "Error when simulating FSM %s: non deterministic transitions found at t=%d: %a\n" 
         f t (Misc.pp_list_v L.Syntax.pp_transition) ts
    | L.Vcd.Unsupported (ty,v) ->
       eprintf "No representation for VCD type/value: %a:%a\n" Vcd_types.pp_vcd_typ ty Vcd_types.pp_vcd_value v; exit 2
    | Misc.Not_implemented msg ->
       eprintf "Not implemented: %s.\n" msg; flush stderr; exit 22
    | Misc.Fatal_error msg ->
       eprintf "Internal error: %s.\n" msg; flush stderr; exit 23
    | Sys_error msg ->
       eprintf "Input/output error: %s.\n" msg; flush stderr; exit 21
    | Sys.Break -> flush stderr; exit 20
    | End_of_file -> exit 0
    | e ->
       if !Options.dump_backtrace then Printexc.print_backtrace stderr;
       L.Guest.Error.handle e
end
