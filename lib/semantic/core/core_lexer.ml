open Core_parser

let id =
  [%sedlex.regexp?
    ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') )]

let rec token buf =
  match%sedlex buf with
  | "let" -> LET
  | "rec" -> REC
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
  | "method" -> METHOD
  | "self" -> SELF
  | "Self" -> SELF_TYPE
  | "true" -> TRUE
  | "false" -> FALSE
  | "->" -> ARROW
  | "()" -> UNIT
  | "==" -> CMP_OP "=="
  | "!=" -> CMP_OP "!="
  | ">=" -> CMP_OP ">="
  | "<=" -> CMP_OP "<="
  | ">" -> CMP_OP ">"
  | "<" -> CMP_OP "<"
  | "+" -> ADD_OP "+"
  | "-" -> ADD_OP "-"
  | "*" -> MUL_OP "*"
  | "/" -> MUL_OP "/"
  | "%" -> MUL_OP "%"
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
  | "'", '\\', 'n', "'" -> CHAR '\n'
  | "'", '\\', 't', "'" -> CHAR '\t'
  | "'", '\\', 'r', "'" -> CHAR '\r'
  | "'", '\\', "'", "'" -> CHAR '\''
  | "'", '\\', '\\', "'" -> CHAR '\\'
  | "'", Compl ('\'' | '\\' | '\n' | '\r'), "'" ->
      let lexeme = Sedlexing.Utf8.lexeme buf in
      CHAR lexeme.[1]
  | id -> ID (Sedlexing.Utf8.lexeme buf)
  | Plus (' ' | '\t' | '\n' | '\r') -> token buf
  | eof -> EOF
  | _ -> failwith ("unexpected token: " ^ Sedlexing.Utf8.lexeme buf)

let parse_expr source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer
