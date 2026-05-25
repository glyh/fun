let rec user_input prompt callback =
  LNoise.linenoise prompt
  |> Option.iter (fun input ->
         callback input;
         user_input prompt callback)

let run source =
  let expr = Core_lexer.parse_expr source in
  let loader = Core_loader.create ~base_dir:(Sys.getcwd ()) in
  let ctx = Elaborate.init_ctx () in
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
