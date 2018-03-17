{
open Main_parser

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
  "event", TYEVENT;
  "int", TYINT;
  "bool", TYBOOL;
]
}

rule main = parse
  | [' ' '\t' '\010' '\013' ] +
      { main !Location.input_lexbuf }
  | ['A'-'Z' 'a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf  in
        try List.assoc s keyword_table
        with Not_found -> ID s }
  | "#"
      { comment !Location.input_lexbuf; main !Location.input_lexbuf }
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
(*   | ['0'-'9']+ ('.' ['0'-'9']*\)? (['e' 'E'] ['+' '-']? ['0'-'9']+)? *)
(*       { FLOAT (float_of_string(Lexing.lexeme !Location.input_lexbuf)) } *)
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
(*   | "[" { LBRACKET } *)
(*   | "]" { RBRACKET } *)
  | "," { COMMA }
  | ".." { DOTDOT }
  | "." { DOT }
  | "--" { ARROW_START }
  | "->" { ARROW_END }
  | ":" { COLON }
  | "=" { EQUAL }
  | ":=" { COLEQ }
  (* | "\\" { BSLASH } *)
  | "|" { BAR }
  (* | "||" { BARBAR } *)
  | "!="    { NOTEQUAL }
  | '>'    { GT }
  | '<'    { LT }
  | ">="    { GTE }
  | "<="    { LTE }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '%' { MOD }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }

and comment = parse
  | "\n"
      { () }
  | _
      { comment !Location.input_lexbuf }
