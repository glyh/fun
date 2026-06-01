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
  let load_macros ctx path =
    let base = Sys.getcwd () in
    let file = Filename.concat base (path ^ ".fun") in
    let module_str = In_channel.with_open_text file In_channel.input_all in
    let module_expr =
      Parse_expand.parse_with Core_parser.module_eof module_str
    in
    match module_expr with
    | Surface.Module { bindings } | Surface.Struct { bindings; _ } ->
      List.iter (fun (binding : Surface.struct_binding) ->
        match binding with
        | Surface.MacroBinding { name; value; public = true } ->
          let elaborate expr =
            let loader = Core_loader.create ~base_dir:base in
            let ctx = Elaborate.init_ctx () in
            let core, _ty = Elaborate.on_expr ~loader ctx expr in
            Elaborate.Ctx.eval ctx core
          in
          let lowered = Parse_expand.expand_lower ~elaborate value in
          let macro_fn = elaborate lowered in
          Expand_ctx.register_macro ctx ~name ~value:macro_fn
        | _ -> ())
        bindings
    | _ -> raise (Invalid_argument ("module expected: " ^ path))
  in
  let expr = Parse_expand.parse_expr ~elaborate ~eval_and_apply ~load_macros source in
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
