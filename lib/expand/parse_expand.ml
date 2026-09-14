(* Wrap [load_syntax] to record the names of the roles an import harvests while
   a source is read, so expansion can find a binder that mixes with one (M7). *)
let recording_imported_roles load_syntax =
  let names = ref [] in
  let record load path =
    let exports = load path in
    names := List.map (fun (op : Binding.operator_info) -> (op.symbol, Some path)) exports @ !names;
    exports
  in
  (Option.map record load_syntax, fun () -> !names)

let expand_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ?(imported_roles = []) stx =
  let ctx = Expand_ctx.create ?loader:None () in
  (match elaborate with Some f -> ctx.elaborate <- Some f | None -> ());
  (match eval_and_apply with Some f -> ctx.eval_and_apply <- Some f | None -> ());
  (match load_macros with Some f -> ctx.load_macros <- Some f | None -> ());
  (match syntax_nominals with Some v -> Expand_ctx.set_syntax_nominals ctx v | None -> ());
  Expand_ctx.add_imported_roles ctx imported_roles;
  let expanded = Expand.expand ctx stx in
  (expanded, ctx)

let parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  let load_syntax, imported_roles = recording_imported_roles load_syntax in
  let stx = Enforest.parse_expr ?load_syntax ?open_prelude source in
  expand_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~imported_roles:(imported_roles ()) stx

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  let expanded, _ctx = parse_expr_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source in
  expanded

let parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source =
  let load_syntax, imported_roles = recording_imported_roles load_syntax in
  let stx = Enforest.parse_module ?load_syntax source in
  expand_syntax ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~imported_roles:(imported_roles ()) stx

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source =
  let expanded, _ctx = parse_module_with_ctx ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source in
  expanded
