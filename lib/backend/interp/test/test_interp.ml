module Testable = struct
  let type_ =
    let pp_type ppf ty = Fmt.pf ppf "%S" (Type.T.pp ty) in
    Alcotest.testable pp_type Type.T.equal

  let value =
    let pp_value ppf ty = Fmt.pf ppf "%S" (Interp.Model.Value.pp ty) in
    Alcotest.testable pp_value Interp.Model.Value.equal
end

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof lexer

let typecheck expr = Typecheck.Inference.on_expr Typecheck.TypeEnv.default expr

let eval expr =
  let result_ty = typecheck expr in
  let result = Interp.(Eval.eval Env.default expr) in
  (result, result_ty)

let test_eval ?(tag = "evaluated as expected") ~source ~expected ~typ () =
  let result = parse_expr source |> eval in
  Alcotest.check
    (Alcotest.pair Testable.value Testable.type_)
    tag result (expected, typ)

let recursion =
  Alcotest.
    [
      test_case "fact 5" `Quick
        (test_eval
           ~source:
             "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) \
              in fact 5"
           ~expected:(Interp.Model.Value.Norm (Syntax.Ast.Atom.I64 120L))
           ~typ:Type.Builtin.i64);
      test_case "fib 6" `Quick
        (test_eval
           ~source:
             "let rec fib = fun n -> if n <= 1 then n else fib (n-1) + fib \
              (n-2) in fib 6"
           ~expected:(Interp.Model.Value.Norm (Syntax.Ast.Atom.I64 8L))
           ~typ:Type.Builtin.i64);
      test_case "apply twice" `Quick
        (test_eval
           ~source:
             "let twice = fun f -> fun x -> f (f x) in let inc = fun n -> n + \
              1 in twice inc 3"
           ~expected:(Interp.Model.Value.Norm (Syntax.Ast.Atom.I64 5L))
           ~typ:Type.Builtin.i64);
    ]

let () = Alcotest.run "Interp" [ ("recursion", recursion) ]
