let expand_lower ?elaborate ?eval_and_apply surface =
  let stx = Surface_to_syntax.expr surface in
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  let expanded = Expand.expand ctx stx in
  Lower_surface.lower_expr expanded

let parse_with parser source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised parser lexer

let parse_expr ?elaborate ?eval_and_apply source =
  parse_with Core_parser.expr_eof source |> expand_lower ?elaborate ?eval_and_apply

let parse_module source =
  parse_with Core_parser.module_eof source |> expand_lower
