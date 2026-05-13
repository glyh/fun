let () =
  let input = "match 1 with x -> 2 end" in
  let lexbuf = Sedlexing.Utf8.from_string input in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  (try
    let _ast = MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer in
    Printf.printf "OK\n"
  with _ -> Printf.printf "FAIL\n");
  let input2 = "type Color = Red in match Red with x -> 1 end" in
  let lexbuf2 = Sedlexing.Utf8.from_string input2 in
  let lexer2 = Sedlexing.with_tokenizer Core_lexer.token lexbuf2 in
  (try
    let _ast2 = MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer2 in
    Printf.printf "OK2\n"
  with _ -> Printf.printf "FAIL2\n")
