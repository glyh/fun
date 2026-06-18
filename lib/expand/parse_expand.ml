let expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_expr_nominal stx =
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  (match load_macros with Some f -> ctx.load_macros <- Some f | None -> ());
  (match syntax_expr_nominal with Some v -> Expand_ctx.set_syntax_expr_nominal ctx v | None -> ());
  let expanded = Expand.expand ctx stx in
  Lower_surface.lower_expr expanded

let expand_lower ?elaborate ?eval_and_apply ?load_macros ?syntax_expr_nominal surface =
  Surface_to_syntax.expr surface |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_expr_nominal

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_expr_nominal source =
  Enforest.parse_expr ?load_syntax source |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_expr_nominal

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_expr_nominal source =
  Enforest.parse_module ?load_syntax source |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_expr_nominal
