open Core_tt

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer

let () =
  let source = "type Option a = Some a | None in \
                fun x -> match x with Some(y) -> Some(Some(y)) | None -> None end" in
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  (try
    ignore (Elaborate.infer ctx expr);
    Printf.printf "OK\n"
  with e ->
    Printf.printf "FAIL: %s\n" (Printexc.to_string e));
  Debug.dump_metas ctx.metas
