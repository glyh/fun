let rec user_input prompt callback =
  LNoise.linenoise prompt
  |> Option.iter (fun v ->
         callback v;
         user_input prompt callback)

let typecheck ?loader expr =
  Typecheck.Inference.on_expr ?loader Typecheck.TypeEnv.default (Syntax.Desugar.expr expr)

let interactive_pipeline source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  let parsed =
    MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof
      lexer
  in
  let loader =
    Loader.create ~base_dir:(Sys.getcwd ())
      ~typecheck_fn:(fun ?loader expr -> typecheck ?loader expr)
  in
  let typed, type_defs =
    typecheck ~loader parsed
  in
  let result = Interp.(Eval.eval ~type_defs Env.default typed) in
  Printf.printf "%s: %s\n"
    (Interp.Model.Value.pp result)
    (Type.T.pp typed.Typed_ir.Expr.type_);
  Out_channel.(flush stdout)

let () = interactive_pipeline |> user_input "fun> "
