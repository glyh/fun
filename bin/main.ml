let rec user_input prompt callback =
  LNoise.linenoise prompt
  |> Option.iter (fun v ->
         callback v;
         user_input prompt callback)

let interactive_pipeline source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  let parsed =
    MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof
      lexer
  in
  let type_inferred =
    Typecheck.Inference.on_expr Typecheck.TypeEnv.default parsed
  in
  let result = Interp.(Eval.eval Env.default parsed) in
  Printf.printf "%s: %s\n"
    (Interp.Model.Value.pp result)
    (Type.T.pp type_inferred);
  Out_channel.(flush stdout)

let () = interactive_pipeline |> user_input "fun> "
