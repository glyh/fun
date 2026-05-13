open Core_parser

let id =
  [%sedlex.regexp?
    ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') )]

let rec token buf =
  match%sedlex buf with
  | "let" -> LET
  | "in" -> IN
  | "fun" -> FUN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "pub" -> PUB
  | "struct" -> STRUCT
  | "match" -> MATCH
  | "with" -> WITH
  | "end" -> END
  | "open" -> OPEN
  | "type" -> TYPE
  | "true" -> TRUE
  | "false" -> FALSE
  | "->" -> ARROW
  | "()" -> UNIT
  | "==" -> OP "=="
  | "!=" -> OP "!="
  | ">=" -> OP ">="
  | "<=" -> OP "<="
  | ">" -> OP ">"
  | "<" -> OP "<"
  | "+" -> OP "+"
  | "-" -> OP "-"
  | "*" -> OP "*"
  | "/" -> OP "/"
  | "%" -> OP "%"
  | "{" -> LBRACE
  | "}" -> RBRACE
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "=" -> EQUALS
  | ":" -> COLON
  | "." -> DOT
  | "," -> COMMA
  | ";" -> SEMI
  | "|" -> BAR
  | Plus '0' .. '9' ->
      INT (Sedlexing.Utf8.lexeme buf |> Int64.of_string)
  | id -> ID (Sedlexing.Utf8.lexeme buf)
  | Plus (' ' | '\t' | '\n' | '\r') -> token buf
  | eof -> EOF
  | _ -> failwith ("unexpected token: " ^ Sedlexing.Utf8.lexeme buf)
