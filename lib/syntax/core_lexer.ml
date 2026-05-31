open Core_parser

type spanned_token = {
  token : Core_parser.token;
  span : Source_span.t;
}

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
  | "module" -> MODULE
  | "sig" -> SIG
  | "struct" -> STRUCT
  | "match" -> MATCH
  | "with" -> WITH
  | "end" -> END
  | "open" -> OPEN
  | "type" -> TYPE
  | "effect" -> EFFECT
  | "trait" -> TRAIT
  | "impl" -> IMPL
  | "can" -> CAN
  | "perform" -> PERFORM
  | "resume" -> RESUME
  | "ref" -> REF
  | "method" -> METHOD
  | "import" -> IMPORT
  | "macro" -> MACRO
  | "self" -> SELF
  | "Self" -> SELF_TYPE
  | "true" -> TRUE
  | "false" -> FALSE
  | "->" -> ARROW
  | "()" -> UNIT
  | "==" -> CMP_OP "=="
  | "!=" -> CMP_OP "!="
  | ":=" -> ASSIGN
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
  | "@" -> AT
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "=" -> EQUALS
  | ":" -> COLON
  | "!" -> BANG
  | "." -> DOT
  | "," -> COMMA
  | ";" -> SEMI
  | "|" -> BAR
  | Plus '0' .. '9' ->
      INT (Sedlexing.Utf8.lexeme buf |> Int64.of_string)
  | '"' -> read_string buf (Buffer.create 16)
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

and read_string buf acc =
  match%sedlex buf with
  | '"' -> STRING (Buffer.contents acc)
  | '\\', '"' ->
      Buffer.add_char acc '"';
      read_string buf acc
  | '\\', 'n' ->
      Buffer.add_char acc '\n';
      read_string buf acc
  | '\\', '\\' ->
      Buffer.add_char acc '\\';
      read_string buf acc
  | eof -> failwith "unterminated string"
  | any ->
      Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
      read_string buf acc
  | _ -> failwith ("unexpected string token: " ^ Sedlexing.Utf8.lexeme buf)

let spanned_token ?file buf =
  let token = token buf in
  let start_pos, end_pos = Sedlexing.lexing_bytes_positions buf in
  let span = Source_span.of_lexing_positions ?file start_pos end_pos in
  { token; span }

let tokens_with_spans ?file source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  Option.iter (Sedlexing.set_filename lexbuf) file;
  let rec loop acc =
    let tok = spanned_token ?file lexbuf in
    match tok.token with
    | EOF -> List.rev (tok :: acc)
    | _ -> loop (tok :: acc)
  in
  loop []
