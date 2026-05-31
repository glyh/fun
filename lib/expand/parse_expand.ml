let expand_lower surface =
  surface
  |> Surface_to_syntax.expr
  |> Expand.expand_expr
  |> Lower_surface.lower_expr

let parse_with parser source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised parser lexer

let parse_expr source =
  parse_with Core_parser.expr_eof source |> expand_lower

let parse_module source =
  parse_with Core_parser.module_eof source |> expand_lower
