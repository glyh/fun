let rec user_input prompt callback =
  LNoise.linenoise prompt
  |> Option.iter (fun input ->
         callback input;
         user_input prompt callback)

let run source =
  let base = Sys.getcwd () in
  let loader = Core_loader.create ~base_dir:base in
  let elaborate expr =
    let ctx = Elaborate.init_ctx () in
    let core, _ty = Elaborate.on_expr ~loader ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  let expr, expand_ctx =
    Parse_expand.parse_expr_with_ctx
      ~elaborate
      ~eval_and_apply
      ~load_macros:(Core_loader.visit_macros loader)
      ~load_syntax:(Core_loader.load_syntax_exports loader)
      source
  in
  let ctx = Elaborate.init_ctx () in
  Hashtbl.iter (fun name value ->
    let kind = match Hashtbl.find_opt expand_ctx.Expand_ctx.macro_kind_table name with
      | Some k -> k | None -> Syntax.MacroKind.default in
    Hashtbl.replace ctx.Elab_ctx.Ctx.macro_table name (value, kind))
    expand_ctx.Expand_ctx.macro_table;
  ctx.Elab_ctx.Ctx.expand_ctx <- Some expand_ctx;
  let core, ty = Elaborate.on_expr ~loader ctx expr in
  let value = Elaborate.Ctx.eval ctx core in
  Printf.printf "%s: %s\n"
    (Debug.pp_value_short ctx.metas value)
    (Debug.pp_value_short ctx.metas ty);
  Out_channel.flush stdout

let interactive_pipeline source =
  try run source
  with exn ->
    Printf.eprintf "error: %s\n%!" (Printexc.to_string exn)

let () = user_input "fun> " interactive_pipeline
