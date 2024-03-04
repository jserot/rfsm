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

(* Lexer  for the host language *)

(* This file will be pre-processed to generate the lexer for the target language *)

{
open Parser

module Location = Rfsm.Location

type lexical_error =
    Illegal_character

exception Lexical_error of lexical_error * int * int

(* The table of keywords *)

let keyword_table = [
  "fsm", FSM;
  "model", MODEL;
  "states", STATES;
  "in", IN;
  "out", OUT;
  "inout", INOUT;
  "vars", VARS;
  "trans", TRANS;
  "itrans", ITRANS;
  "input", INPUT;
  "output", OUTPUT;
  "shared", SHARED;
  "periodic", PERIODIC;
  "sporadic", SPORADIC;
  "value_changes", VALUE_CHANGES;
  "on", ON;
  "when", WHEN;
  "with", WITH;
  "where", WHERE;
  "and", AND;
  "type", TYPE;
  "constant", CONSTANT;
  "function", FUNCTION;
  "return", RETURN;
  "type", TYPE;
  (* For program fragments *)
  "var", VAR;
  "guard", GUARD;
  "action", ACTION;
  "sval", SVAL;
#include "guest_kw.mll"
]

type token = Parser.token
}

rule main = parse
  | [' ' '\t' '\010' '\013' ] +
      { main !Location.input_lexbuf }
  | ['a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf  in
        try List.assoc s keyword_table
        with Not_found -> LID s }
  | ['A'-'Z' 'a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { UID (Lexing.lexeme !Location.input_lexbuf) }
  | "--"
      { comment !Location.input_lexbuf; main !Location.input_lexbuf }
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "<" { LT }
  | ">" { GT }
  | "," { COMMA }
  | "." { DOT }
  | "->" { ARROW }
  | ":" { COLON }
  | ":=" { COLEQ }
  | "|" { BAR }
  | "!" { EMARK }
  | "=" { EQUAL }
#include "guest_rules.mll"
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }

and comment = parse
  | "\n"
      { () }
  | _
      { comment !Location.input_lexbuf }
