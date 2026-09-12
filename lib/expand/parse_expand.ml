let expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ?expansion_position stx =
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  (match load_macros with Some f -> ctx.load_macros <- Some f | None -> ());
  (match syntax_nominals with Some v -> Expand_ctx.set_syntax_nominals ctx v | None -> ());
  (match expansion_position with Some v -> Expand_ctx.set_expansion_position ctx v | None -> ());
  let expanded = Expand.expand ctx stx in
  (Lower_surface.lower_expr expanded, ctx)

let parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals ?expansion_position source =
  let expansion_position = match expansion_position with Some k -> k | None -> Syntax.MacroKind.(Expr (None, None)) in
  Enforest.parse_expr ?load_syntax ?open_prelude source |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~expansion_position

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals ?expansion_position source =
  let surface, _ctx = parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals ?expansion_position source in
  surface

let parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals ?expansion_position source =
  let expansion_position = match expansion_position with Some k -> k | None -> Syntax.MacroKind.Decl in
  Enforest.parse_module ?load_syntax source |> expand_lower_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~expansion_position

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals ?expansion_position source =
  let surface, _ctx = parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals ?expansion_position source in
  surface
