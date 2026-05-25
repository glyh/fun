let () =
  let input = "match 1 with x -> 2 end" in
  (try
    let _ast = Core_lexer.parse_expr input in
    Printf.printf "OK\n"
  with _ -> Printf.printf "FAIL\n");
  let input2 = "type Color = Red in match Red with x -> 1 end" in
  (try
    let _ast2 = Core_lexer.parse_expr input2 in
    Printf.printf "OK2\n"
  with _ -> Printf.printf "FAIL2\n")
