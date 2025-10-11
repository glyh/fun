module Testable = struct
  let type_ =
    let pp_binding ppf expr = Fmt.pf ppf "%S" (Type.T.pp expr) in
    Alcotest.testable pp_binding Type.T.equal
end

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof lexer

let test_typecheck ?(tag = "same type") ~source ~expected () =
  let expr = parse_expr source in
  let inferred_type = Typecheck.Inference.on_type Type.Id.Map.empty expr in
  Alcotest.(check Testable.type_) tag inferred_type expected

let constants =
  Alcotest.
    [
      test_case "single int" `Quick
        (test_typecheck ~source:"42" ~expected:Type.Builtin.int);
      test_case "unit literal" `Quick
        (test_typecheck ~source:"()" ~expected:Type.Builtin.unit);
    ]

let let_bindings =
  Alcotest.
    [
      test_case "simple let" `Quick
        (test_typecheck ~source:"let x = 1 in x" ~expected:Type.Builtin.int);
    ]

let lambdas =
  Alcotest.
    [
      test_case "lambda application" `Quick
        (test_typecheck ~source:"(fun x -> x) 3" ~expected:Type.Builtin.int);
    ]

let () =
  Alcotest.run "Typecheck"
    [
      ("constants", constants);
      ("let_bindings", let_bindings);
      ("lambdas", lambdas);
    ]
