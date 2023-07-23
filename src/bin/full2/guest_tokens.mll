  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme !Location.input_lexbuf)) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "." { DOT }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | "%"  { MOD }
  | "!=" { NOTEQUAL }
  | '%' { MOD }
  | '&' { LAND }
  | "||" { LOR }
  | '^' { LXOR }
  | ">>" { SHR }
  | "<<" { SHL }
  | ">="    { GTE }
  | "<="    { LTE }
  | "+." { FPLUS }
  | "-." { FMINUS }
  | "*." { FTIMES }
  | "/." { FDIV }
  | "?" { QMARK }
  | "::" { COLONCOLON }
