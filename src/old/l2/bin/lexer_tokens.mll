  | ['a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { let s = Lexing.lexeme !Location.input_lexbuf  in
        try List.assoc s keyword_table
        with Not_found -> LID s }
  | ['A'-'Z' 'a'-'z' ] ( ['A'-'Z' 'a'-'z' '0'-'9' '_' ] ) *
      { UID (Lexing.lexeme !Location.input_lexbuf) }
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | ":"    { COLON }
  | ":="    { COLEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "=" { EQUAL }
  | "!=" { NOTEQUAL }
  | ">" { GT }
  | "<" { LT }
  | "," { COMMA }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
