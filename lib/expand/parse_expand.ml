let expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros stx =
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  (match load_macros with Some f -> ctx.load_macros <- Some f | None -> ());
  let expanded = Expand.expand ctx stx in
  Lower_surface.lower_expr expanded

let expand_lower ?elaborate ?eval_and_apply ?load_macros surface =
  Surface_to_syntax.expr surface |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros

let parse_with parser source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised parser lexer

let parse_expr ?elaborate ?eval_and_apply ?load_macros source =
  try
    Enforest.parse_expr source |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros
  with Enforest.Unsupported _ ->
    parse_with Core_parser.expr_eof source |> expand_lower ?elaborate ?eval_and_apply ?load_macros

let parse_module ?elaborate ?eval_and_apply ?load_macros source =
  parse_with Core_parser.module_eof source |> expand_lower ?elaborate ?eval_and_apply ?load_macros
