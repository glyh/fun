let () =
  Printexc.register_printer (function
    | Elaborate.ElabError e ->
        let open Elaborate in
        Some (Printf.sprintf "ElabError(%s)" (match e with
          | UnboundVariable n -> "UnboundVariable " ^ n
          | ApplyingNonFunction -> "ApplyingNonFunction"
          | _ -> "other"))
    | _ -> None)

let rec user_input prompt callback =
  LNoise.linenoise prompt
  |> Option.iter (fun input ->
         callback input;
         user_input prompt callback)

let run source =
  let elaborate expr =
    let loader = Core_loader.create ~base_dir:(Sys.getcwd ()) in
    let ctx = Elaborate.init_ctx () in
    let core, _ty = Elaborate.on_expr ~loader ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  let expr = Parse_expand.parse_expr ~elaborate ~eval_and_apply source in
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
