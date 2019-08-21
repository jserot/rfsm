{
open Main_parser

type lexical_error =
    Illegal_character

exception Lexical_error of lexical_error * int * int

(* The table of keywords *)

let keyword_table = [
  "type", TYPE;
  "enum", ENUM;
  "record", RECORD;
  "function", FUNCTION;
  "constant", CONSTANT;
  "return", RETURN;
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
  "float", TYFLOAT;
  "bool", TYBOOL;
  "char", TYCHAR;
  "array", TYARRAY;
  "on", ON;
  "when", WHEN;
  "with", WITH;
  (* "true", TRUE;
   * "false", FALSE; *)
]
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
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "," { COMMA }
  | "." { DOT }
  | "->" { ARROW }
  | ":" { COLON }
  | "?" { QMARK }
  | "!" { EMARK }
  | "=" { EQUAL }
  | ":=" { COLEQ }
  | "|" { BAR }
  | "!="    { NOTEQUAL }
  | '>'    { GT }
  | '<'    { LT }
  | ">="    { GTE }
  | "<="    { LTE }
  | "+." { FPLUS }
  | "-." { FMINUS }
  | "*." { FTIMES }
  | "/." { FDIV }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MOD }
  | '&' { LAND }
  | "||" { LOR }
  | '^' { LXOR }
  | ">>" { SHR }
  | "<<" { SHL }
  | "::" { COLONCOLON }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start !Location.input_lexbuf, Lexing.lexeme_end !Location.input_lexbuf)) }

and comment = parse
  | "\n"
      { () }
  | _
      { comment !Location.input_lexbuf }
