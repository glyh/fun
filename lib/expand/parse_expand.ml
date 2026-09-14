let expand_syntax ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals stx =
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  (match load_macros with Some f -> ctx.load_macros <- Some f | None -> ());
  ctx.load_syntax <- load_syntax;
  (match syntax_nominals with Some v -> Expand_ctx.set_syntax_nominals ctx v | None -> ());
  let expanded = Expand.expand ctx stx in
  (expanded, ctx)

let parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  Enforest.parse_expr ?open_prelude source
  |> expand_syntax ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  fst (parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source)

let parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals ?file source =
  Enforest.parse_module ?file source
  |> expand_syntax ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source =
  fst (parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source)

(* A unit's syntax exports: the public roles its expansion declares. *)
let syntax_exports ?load_syntax ?file source =
  (snd (parse_module_with_ctx ?load_syntax ?file source)).Expand_ctx.syntax_exports
