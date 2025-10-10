open Ast

let parse_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.toplevel_eof
  in
  try parser lexer
  with Lexer.UnexpectedToken { token; location = loc_start, loc_end } ->
    failwith
      (Printf.sprintf "UnexpectedToken `%s` at offset (%d, %d)." token loc_start
         loc_end)

let%test "simple let binding" =
  let input = "let x = 42" in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value = Expr.Num 42 } ]

let%test "multiple let bindings" =
  let input = "let x = 1 ;; let y = 2" in
  List.equal Binding.equal (parse_string input)
    [
      { recursive = false; name = "x"; type_ = None; value = Expr.Num 1 };
      { recursive = false; name = "y"; type_ = None; value = Expr.Num 2 };
    ]

let%test "top level binding no params" =
  let input = "let f = 42" in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = Expr.Num 42 } ]

let%test "function with untyped params" =
  let input = "let f x y = x" in
  let lam_y = Expr.Lam (Param.{ name = "y"; type_ = None }, Expr.Var "x") in
  let lam_x = Expr.Lam (Param.{ name = "x"; type_ = None }, lam_y) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam_x } ]

let%test "function with typed params" =
  let input = "let f (x:int) (y:bool) = x" in
  let lam_y =
    Expr.Lam (Param.{ name = "y"; type_ = Some "bool" }, Expr.Var "x")
  in
  let lam_x = Expr.Lam (Param.{ name = "x"; type_ = Some "int" }, lam_y) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam_x } ]

let%test "nested let in" =
  let input = "let x = let y = 1 in y" in
  let inner =
    Expr.Let
      {
        binding =
          { recursive = false; name = "y"; type_ = None; value = Expr.Num 1 };
        body = Expr.Var "y";
      }
  in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value = inner } ]

let%test "recursive let binding" =
  let input = "let rec f = fun x -> x" in
  let value = Expr.Lam (Param.{ name = "x"; type_ = None }, Expr.Var "x") in
  List.equal Binding.equal (parse_string input)
    [ { recursive = true; name = "f"; type_ = None; value } ]

let%test "application with parentheses" =
  let input = "let x = (f y) z" in
  let value = Expr.Ap (Expr.Ap (Expr.Var "f", Expr.Var "y"), Expr.Var "z") in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value } ]

let%test "if expression with parentheses" =
  let input = "let x = if (1) then 2 else 3" in
  let cond = Expr.Num 1 in
  let value = Expr.If { cond; then_ = Expr.Num 2; else_ = Expr.Num 3 } in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value } ]

let%test "multiple function bindings" =
  let input = "let f x = x ;; let g y = y" in
  let lam_f = Expr.Lam (Param.{ name = "x"; type_ = None }, Expr.Var "x") in
  let lam_g = Expr.Lam (Param.{ name = "y"; type_ = None }, Expr.Var "y") in
  List.equal Binding.equal (parse_string input)
    [
      { recursive = false; name = "f"; type_ = None; value = lam_f };
      { recursive = false; name = "g"; type_ = None; value = lam_g };
    ]

let%test "application left associativity" =
  let input = "let x = a b c" in
  let value = Expr.Ap (Expr.Ap (Expr.Var "a", Expr.Var "b"), Expr.Var "c") in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value } ]

let%test "function with mixed typed params" =
  let input = "let f (x:int) y = y" in
  let lam_y = Expr.Lam (Param.{ name = "y"; type_ = None }, Expr.Var "y") in
  let lam_x = Expr.Lam (Param.{ name = "x"; type_ = Some "int" }, lam_y) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam_x } ]

let%test "nested lambdas" =
  let input = "let f = fun x -> fun y -> x" in
  let lam_y = Expr.Lam (Param.{ name = "y"; type_ = None }, Expr.Var "x") in
  let lam_x = Expr.Lam (Param.{ name = "x"; type_ = None }, lam_y) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam_x } ]

let%test "deeply nested parentheses" =
  let input = "let x = (((42)))" in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value = Expr.Num 42 } ]

let%test "recursive function with params" =
  let input = "let rec f x = f x" in
  let value =
    Expr.Lam
      (Param.{ name = "x"; type_ = None }, Expr.Ap (Expr.Var "f", Expr.Var "x"))
  in
  List.equal Binding.equal (parse_string input)
    [ { recursive = true; name = "f"; type_ = None; value } ]

let%test "nested if in else branch" =
  let input = "let x = if 1 then 2 else if 3 then 4 else 5" in
  let inner =
    Expr.If { cond = Expr.Num 3; then_ = Expr.Num 4; else_ = Expr.Num 5 }
  in
  let outer =
    Expr.If { cond = Expr.Num 1; then_ = Expr.Num 2; else_ = inner }
  in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value = outer } ]

let%test "function returning function" =
  let input = "let f x = fun y -> y" in
  let body = Expr.Lam (Param.{ name = "y"; type_ = None }, Expr.Var "y") in
  let lam = Expr.Lam (Param.{ name = "x"; type_ = None }, body) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam } ]

let%test "application right associativity with parentheses" =
  let input = "let x = a (b c)" in
  let value = Expr.Ap (Expr.Var "a", Expr.Ap (Expr.Var "b", Expr.Var "c")) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value } ]

let%test "nested let inside function body" =
  let input = "let f x = let y = x in y" in
  let inner =
    Expr.Let
      {
        binding =
          { recursive = false; name = "y"; type_ = None; value = Expr.Var "x" };
        body = Expr.Var "y";
      }
  in
  let lam = Expr.Lam (Param.{ name = "x"; type_ = None }, inner) in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "f"; type_ = None; value = lam } ]

let%test "application of function call result" =
  let input = "let x = (f x) (g y)" in
  let value =
    Expr.Ap
      ( Expr.Ap (Expr.Var "f", Expr.Var "x"),
        Expr.Ap (Expr.Var "g", Expr.Var "y") )
  in
  List.equal Binding.equal (parse_string input)
    [ { recursive = false; name = "x"; type_ = None; value } ]

let%test "recursive function with mixed typed params" =
  let input = "let rec f (x:int) y = f y" in
  let value =
    Expr.Lam
      ( Param.{ name = "x"; type_ = Some "int" },
        Expr.Lam
          ( Param.{ name = "y"; type_ = None },
            Expr.Ap (Expr.Var "f", Expr.Var "y") ) )
  in
  List.equal Binding.equal (parse_string input)
    [ { recursive = true; name = "f"; type_ = None; value } ]
