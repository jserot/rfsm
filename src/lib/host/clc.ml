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

  let usage = "usage: rfsmc [options...] files"

  let source_files = ref ([] : string list)

  let anonymous fname = source_files := !source_files @ [fname]

  let print_banner () = 
    Printf.printf "---------------------------------------------------------------------------\n";
    Printf.printf "Reactive Finite State Machine compiler and simulator, version %s-%s/%s\n"
      L.Guest.Info.name L.Guest.Info.version Version.version;
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
    let open L in
    let p0 =
      List.fold_left
        (fun p f -> Syntax.add_program p (parse f))
        Syntax.empty_program
        !source_files in
    let p = Syntax.ppr_program p0 in
    (* Format.printf "parsed=%a" pp_program p; *)
    let tenv0 = Typing.mk_env () in
    if !Options.dump_tenv then Format.printf "tenv=%a" pp_tenv tenv0;
    type_program tenv0 p;
    if !Options.dump_typed then Format.printf "tp=%a" pp_program p;
    let s = elab p in
    if !Options.dump_static then Format.printf "s=%a" (Static.pp ~verbose_level:2) s;
    Logfile.start ();
    begin match !Options.target with
    | Some Options.Dot ->
       Misc.check_dir !Options.target_dir;
       let fs =
         Dot.output_static 
           ~dir:!Options.target_dir
           ~name:!Options.main_prefix
           ~with_models:!Options.dot_show_models
           ~with_caption:!Options.dot_captions
           s in
       List.iter Logfile.write fs
    | Some Options.CTask ->
       (* let cms = List.map Cmodel.c_model_of_fsm_model s.Static.models in
        * List.iter (fun m -> Format.printf "%a@." Cmodel.pp m) cms *)
       (* Ctask.check_allowed s; *)
       Misc.check_dir !Options.target_dir;
       let fs = Ctask.output ~dir:!Options.target_dir s in
       List.iter Logfile.write fs
    | Some Options.SystemC ->
       Misc.check_dir !Options.target_dir;
       let fs = Systemc.output ~dir:!Options.target_dir ~pfx:!Options.main_prefix s in
       List.iter Logfile.write fs
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
         run ~vcd_file p s
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
    | L.Syntax.Undefined_symbol (loc,s)
    | L.Typing.Undefined_symbol (loc,s) -> 
       eprintf "%aUndefined symbol: %a\n" pp_location loc Ident.pp s; exit 2
    | L.Typing.Duplicate_symbol (loc,s) -> 
       eprintf "%aThe symbol %a is already defined in this context\n" pp_location loc Ident.pp s; exit 2
    | L.Typing.Duplicate_state (loc,name) ->
       eprintf "%aDuplicate state name: %a\n" pp_location loc Ident.pp name; exit 2
    | L.Typing.Invalid_state (loc,name) ->
       eprintf "%aNo state named %a\n" pp_location loc Ident.pp name; exit 2
    | L.Typing.Illegal_inst loc ->
       eprintf "%aCannot instantiate model: formal and actual parameters do not match\n" pp_location loc; exit 2
    | L.Typing.No_event_input loc ->
       eprintf "%aThere must be at least one input with type event for this model\n" pp_location loc; exit 2
    | L.Typing.Illegal_state_output (loc,q,o) ->
       eprintf "%aIllegal valuation for output %a in state %a\n" pp_location loc Ident.pp o Ident.pp q; exit 2
    | L.Dynamic.Illegal_stimulus_value loc ->
       eprintf "%aIllegal stimulus value\n" pp_location loc; exit 2
    | L.Dynamic.Non_deterministic_transition (f, t, ts) ->
       eprintf "Error when simulating FSM %s: non deterministic transitions found at t=%d: %a\n" 
         f t (Misc.pp_list_v L.Syntax.pp_transition) ts
    | L.Guest.Value.Unsupported_vcd v ->
       eprintf "No VCD conversion for value %a\n" L.Guest.Value.pp v; exit 2
    | L.Guest.Static.Non_static_value e ->
       eprintf "%aThis expression cannot be statically evaluated\n" pp_location e.Annot.loc; exit 2
    | L.Vcd.Unsupported (ty,v) ->
       eprintf "No representation for VCD type/value: %a:%a\n" Vcd_types.pp_vcd_typ ty Vcd_types.pp_vcd_value v; exit 2
    (* | L.Systemc.Invalid_output_assign (id,loc) ->
     *    eprintf "%aSystemC backend; cannot assign non-scalar output %s\n" loc id; exit 2 *)
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
