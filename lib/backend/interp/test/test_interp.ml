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
  let result = Interp.Eval.(eval default_env expr) in
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
    ]

let () = Alcotest.run "Interp" [ ("recursion", recursion) ]
