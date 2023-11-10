  | "true"  { BOOL(true) }
  | "false"  { BOOL(false) }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | "!=" { NOTEQUAL }
