type delimiter = Paren | Bracket | Brace

type token_kind =
  | Ident of string
  | Int of int64
  | Char of char
  | String of string
  | Unit
  | KwLet
  | KwFun
  | KwThen
  | KwSig
  | KwFn
  | KwDo
  | KwEnd
  | KwIf
  | KwElse
  | KwMatch
  | KwWith
  | KwEffect
  | KwType
  | KwModule
  | KwStruct
  | KwImpl
  | KwTrait
  | KwPub
  | KwImport
  | KwOpen
  | KwMacro
  | KwPattern
  | KwSelf
  | KwSelfType
  | KwRef
  | KwDeref
  | KwRec
  | KwCan
  | KwPerform
  | KwResume
  | KwMethod
  | KwTrue
  | KwFalse
  | KwUnit
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Colon
  | Equals
  | Semi
  | Bar
  | ThinArrow
  | At
  | DatumComment
  | Operator of string
  | Eof

type token = {
  kind : token_kind;
  span : Source_span.t;
}

type datum =
  | Token of token
  | Group of delimiter * t list * Source_span.t

and t = {
  datum : datum;
  span : Source_span.t;
}

exception Error of string

let token kind span = { kind; span }

let syntax_token kind span = { datum = Token (token kind span); span }

let group delimiter items span = { datum = Group (delimiter, items, span); span }

let id =
  [%sedlex.regexp?
    ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '?' | '!') )]

let operator_chars = [%sedlex.regexp? Plus ('+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' | '@' | '~')]

let keyword = function
  | "let" -> Some KwLet
  | "fun" -> Some KwFun
  | "then" -> Some KwThen
  | "sig" -> Some KwSig
  | "fn" -> Some KwFn
  | "do" -> Some KwDo
  | "end" -> Some KwEnd
  | "if" -> Some KwIf
  | "else" -> Some KwElse
  | "match" -> Some KwMatch
  | "with" -> Some KwWith
  | "effect" -> Some KwEffect
  | "type" -> Some KwType
  | "module" -> Some KwModule
  | "struct" -> Some KwStruct
  | "impl" -> Some KwImpl
  | "trait" -> Some KwTrait
  | "pub" -> Some KwPub
  | "import" -> Some KwImport
  | "open" -> Some KwOpen
  | "macro" -> Some KwMacro
  | "pattern" -> Some KwPattern
  | "self" -> Some KwSelf
  | "Self" -> Some KwSelfType
  | "ref" -> Some KwRef
  | "deref" -> Some KwDeref
  | "rec" -> Some KwRec
  | "can" -> Some KwCan
  | "perform" -> Some KwPerform
  | "resume" -> Some KwResume
  | "method" -> Some KwMethod
  | "true" -> Some KwTrue
  | "false" -> Some KwFalse
  | "Unit" -> Some KwUnit
  | _ -> None

let span_between (a : Source_span.t) (b : Source_span.t) =
  if a.synthetic || b.synthetic then Source_span.synthetic
  else
    Source_span.make ?file:a.file ~start_byte:a.start_byte ~end_byte:b.end_byte
      ?start_line:a.start_line ?start_col:a.start_col ?end_line:b.end_line
      ?end_col:b.end_col ()

let rec raw_token buf =
  match%sedlex buf with
  | Plus (' ' | '\t' | '\r') -> raw_token buf
  | '\\', '\n' -> raw_token buf
  | '\n' -> Semi
  | "#|" ->
      skip_block_comment 1 buf;
      raw_token buf
  | "#_" -> DatumComment
  | "#" -> skip_line_comment buf
  | "->" -> ThinArrow
  | "<-" -> Operator "<-"
  | "==" -> Operator "=="
  | "!=" -> Operator "!="
  | ">=" -> Operator ">="
  | "<=" -> Operator "<="
  | "(" -> LParen
  | ")" -> RParen
  | "[" -> LBracket
  | "]" -> RBracket
  | "{" -> LBrace
  | "}" -> RBrace
  | "," -> Comma
  | "." -> Dot
  | ":" -> Colon
  | "=" -> Equals
  | ";" -> Semi
  | "|" -> Bar
  | "@" -> At
  | Plus '0' .. '9' -> Int (Sedlexing.Utf8.lexeme buf |> Int64.of_string)
  | '"' -> read_string buf (Buffer.create 16)
  | "'", '\\', 'n', "'" -> Char '\n'
  | "'", '\\', 't', "'" -> Char '\t'
  | "'", '\\', 'r', "'" -> Char '\r'
  | "'", '\\', "'", "'" -> Char '\''
  | "'", '\\', '\\', "'" -> Char '\\'
  | "'", Compl ('\'' | '\\' | '\n' | '\r'), "'" ->
      let lexeme = Sedlexing.Utf8.lexeme buf in
      Char lexeme.[1]
  | id ->
      let text = Sedlexing.Utf8.lexeme buf in
      (match keyword text with Some kind -> kind | None -> Ident text)
  | "$" -> Operator "$"
  | operator_chars -> Operator (Sedlexing.Utf8.lexeme buf)
  | eof -> Eof
  | _ -> raise (Error ("unexpected character: " ^ Sedlexing.Utf8.lexeme buf))

and read_string buf acc =
  match%sedlex buf with
  | '"' -> String (Buffer.contents acc)
  | '\\', '"' ->
      Buffer.add_char acc '"';
      read_string buf acc
  | '\\', 'n' ->
      Buffer.add_char acc '\n';
      read_string buf acc
  | '\\', 't' ->
      Buffer.add_char acc '\t';
      read_string buf acc
  | '\\', 'r' ->
      Buffer.add_char acc '\r';
      read_string buf acc
  | '\\', '\\' ->
      Buffer.add_char acc '\\';
      read_string buf acc
  | eof -> raise (Error "unterminated string")
  | any ->
      Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
      read_string buf acc
  | _ -> raise (Error ("unexpected string token: " ^ Sedlexing.Utf8.lexeme buf))

and skip_line_comment buf =
  match%sedlex buf with
  | '\n' -> Semi
  | eof -> Eof
  | any -> skip_line_comment buf
  | _ -> raise (Error ("unexpected comment token: " ^ Sedlexing.Utf8.lexeme buf))

and skip_block_comment depth buf =
  match%sedlex buf with
  | "#|" -> skip_block_comment (depth + 1) buf
  | "|#" -> if depth > 1 then skip_block_comment (depth - 1) buf
  | eof -> raise (Error "unterminated block comment")
  | any -> skip_block_comment depth buf
  | _ -> raise (Error ("unexpected block comment token: " ^ Sedlexing.Utf8.lexeme buf))

let spanned_raw_token ?file buf =
  let kind = raw_token buf in
  let start_pos, end_pos = Sedlexing.lexing_bytes_positions buf in
  let span = Source_span.of_lexing_positions ?file start_pos end_pos in
  token kind span

let raw_tokens_with_spans ?file source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  Option.iter (Sedlexing.set_filename lexbuf) file;
  let rec loop acc =
    let tok = spanned_raw_token ?file lexbuf in
    match tok.kind with
    | Eof -> List.rev (tok :: acc)
    | _ -> loop (tok :: acc)
  in
  loop []

let read ?file source =
  let token_name = function
    | LParen -> "("
    | RParen -> ")"
    | LBracket -> "["
    | RBracket -> "]"
    | LBrace -> "{"
    | RBrace -> "}"
    | Comma -> ","
    | Dot -> "."
    | Colon -> ":"
    | Equals -> "="
    | Semi -> ";"
    | Bar -> "|"
    | ThinArrow -> "->"
    | At -> "@"
    | DatumComment -> "#_"
    | Ident s | Operator s -> s
    | KwLet -> "let"
    | KwFun -> "fun"
    | KwThen -> "then"
    | KwSig -> "sig"
    | KwFn -> "fn"
    | KwDo -> "do"
    | KwEnd -> "end"
    | KwIf -> "if"
    | KwElse -> "else"
    | KwMatch -> "match"
    | KwWith -> "with"
    | KwEffect -> "effect"
    | KwType -> "type"
    | KwModule -> "module"
    | KwStruct -> "struct"
    | KwImpl -> "impl"
    | KwTrait -> "trait"
    | KwPub -> "pub"
    | KwImport -> "import"
    | KwOpen -> "open"
    | KwMacro -> "macro"
    | KwPattern -> "pattern"
    | KwSelf -> "self"
    | KwSelfType -> "Self"
    | KwRef -> "ref"
    | KwDeref -> "deref"
    | KwRec -> "rec"
    | KwCan -> "can"
    | KwPerform -> "perform"
    | KwResume -> "resume"
    | KwMethod -> "method"
    | KwTrue -> "true"
    | KwFalse -> "false"
    | KwUnit -> "Unit"
    | Int _ -> "integer"
    | Char _ -> "char"
    | String _ -> "string"
    | Unit -> "()"
    | Eof -> "EOF"
  in
  let rec read_sequence close acc = function
    | [] -> raise (Error "raw token stream ended without EOF")
    | { kind = Eof; _ } :: rest -> (
        match close with
        | None -> (List.rev acc, rest, None)
        | Some expected -> raise (Error ("unterminated " ^ token_name expected ^ " group")))
    | { kind = DatumComment; _ } :: rest ->
        let _skipped, rest = read_one rest in
        read_sequence close acc rest
    | ({ kind; _ } as tok) :: rest when Option.equal (=) close (Some kind) ->
        (List.rev acc, rest, Some tok.span)
    | ({ kind = LParen | LBracket | LBrace; _ } as tok) :: rest ->
        let item, rest = read_group tok rest in
        read_sequence close (item :: acc) rest
    | ({ kind = (RParen | RBracket | RBrace); _ } as tok) :: _ ->
        raise (Error ("unexpected closing delimiter: " ^ token_name tok.kind))
    | tok :: rest -> read_sequence close (syntax_token tok.kind tok.span :: acc) rest
  and read_group opener rest =
    let delimiter, close =
      match opener.kind with
      | LParen -> (Paren, RParen)
      | LBracket -> (Bracket, RBracket)
      | LBrace -> (Brace, RBrace)
      | _ -> raise (Error "internal reader error: expected opening delimiter")
    in
    let items, rest, close_span = read_sequence (Some close) [] rest in
    let close_span = match close_span with Some span -> span | None -> opener.span in
    (group delimiter items (span_between opener.span close_span), rest)
  and read_one = function
    | [] -> raise (Error "expected term")
    | { kind = Eof; _ } :: _ -> raise (Error "expected term")
    | { kind = DatumComment; _ } :: rest ->
        let _skipped, rest = read_one rest in
        read_one rest
    | ({ kind = LParen | LBracket | LBrace; _ } as opener) :: rest -> read_group opener rest
    | ({ kind = (RParen | RBracket | RBrace); _ } as tok) :: _ ->
        raise (Error ("unexpected closing delimiter: " ^ token_name tok.kind))
    | tok :: rest -> (syntax_token tok.kind tok.span, rest)
  in
  let items, _rest, _ = read_sequence None [] (raw_tokens_with_spans ?file source) in
  items
