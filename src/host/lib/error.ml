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

(**{1 Error handling} *)

let pp_loc fmt loc = 
  if !Options.server_mode then ()
  (* else if !Options.gui then Format.fprintf fmt "* Where: \"%s\"\n* Reason: " (Location.text_of_location loc) *)
  else Location.pp_location fmt loc

(** Output signature of the functor {!Error.Make} *)
module type T = sig
  val handle: exn -> unit
end

module type PARSER = sig
  exception Error
end

module type LEXER = sig
  type lexical_error = Illegal_character
  exception Lexical_error of lexical_error * int * int
end

(** Building functor  *)
module Make (L: Host.T) (Lexer: LEXER) (Parser: PARSER) : T =
struct

let handle e =
  let open Location in
  let open Format in
  match e with
    | Parser.Error ->
       let pos1 = Lexing.lexeme_start !input_lexbuf in
       let pos2 = Lexing.lexeme_end !input_lexbuf in
       let loc = Loc(!input_name,pos1, pos2) in
       eprintf "%aSyntax error\n" pp_loc loc;
       flush stderr;
       exit 1
    | Lexer.Lexical_error(Lexer.Illegal_character, pos1, pos2) ->
       eprintf "%aIllegal character.\n" pp_loc (Loc(!input_name,pos1, pos2)); flush stderr; exit 1
    | Ident.Undefined (what,loc,s) ->
       eprintf "%aUndefined %s: %a\n" pp_loc loc what Ident.pp s;
       exit 2
    | Ident.Duplicate (what,loc,x) -> 
      eprintf "%aDuplicate %s: %a\n" pp_loc loc what Ident.pp x; exit 2
    | L.Syntax.Invalid_symbol(x,loc,reason) ->
       eprintf "%aInvalid symbol reference: \"%a\" %s\n" pp_loc loc Ident.pp x reason;
       exit 2
    | L.Typing.Duplicate_symbol (loc,s) -> 
       eprintf "%aThe symbol %a is already defined in this context\n" pp_loc loc Ident.pp s;
       exit 2
    | L.Typing.Duplicate_state (loc,name) ->
       eprintf "%aDuplicate state name: %a\n" pp_loc loc Ident.pp name;
       exit 2
    | L.Typing.Invalid_state (loc,name) ->
       eprintf "%aNo state named %a\n" pp_loc loc Ident.pp name;
       exit 2
    | L.Typing.Illegal_inst loc ->
       eprintf "%aCannot instantiate model: formal and actual parameters do not match\n" pp_loc loc;
       exit 2
    | L.Typing.No_event_input loc ->
       eprintf "%aThere must be at least one input with type event for this model\n" pp_loc loc;
       exit 2
    | L.Typing.Illegal_state_output (loc,q,o) ->
       eprintf "%aIllegal valuation for output %a in state %a\n" pp_loc loc Ident.pp o Ident.pp q;
       exit 2
    | L.Typing.Type_mismatch (loc,t,t') ->
       eprintf "%aType mismatch: the expected type here was %s, not %a\n" pp_loc loc t L.Typing.HostSyntax.pp_typ t' ;
       exit 2
    | L.Dynamic.Illegal_stimulus_value loc ->
       eprintf "%aIllegal stimulus value\n" pp_loc loc;
       exit 2
    | L.Dynamic.Non_deterministic_transition (f, t, ts) ->
       eprintf "Error when simulating FSM %s: non deterministic transitions found at t=%d: %a\n" 
         f t (Ext.List.pp_v L.Syntax.ppf_transition) ts;
       exit 2
    | L.Guest.Value.Unsupported_vcd v ->
       eprintf "No VCD conversion for value %a\n" L.Guest.Value.pp v;
       exit 2
    | L.Guest.Static.Non_static_value e ->
       eprintf "%aThis expression cannot be statically evaluated\n" pp_loc e.Annot.loc;
       exit 2
    | L.Vcd.Unsupported (ty,v) ->
       eprintf "No representation for VCD type/value: %a:%a\n" Vcd_types.pp_vcd_typ ty Vcd_types.pp_vcd_value v;
       exit 2
    | L.Systemc.Invalid_output_assign (id,loc) ->
       eprintf "%aSystemC backend; cannot assign non-scalar output %s\n" pp_loc loc id;
       exit 2
    | L.Vhdl.Invalid_output_assign (id,loc) ->
       eprintf "%aVHDL backend; cannot assign non-scalar output %s\n" pp_loc loc id;
       exit 2
    | Misc.Not_implemented msg ->
       eprintf "Not implemented: %s.\n" msg; flush stderr;
       exit 22
    | Misc.Fatal_error msg ->
       eprintf "Internal error: %s.\n" msg; flush stderr;
       exit 23
    | Sys_error msg ->
       eprintf "Input/output error: %s.\n" msg; flush stderr;
       exit 21
    | Sys.Break -> flush stderr; exit 20
    | End_of_file -> exit 0
    | e ->
       if !Options.dump_backtrace then Printexc.print_backtrace stderr;
       L.Guest.Error.handle e

end
