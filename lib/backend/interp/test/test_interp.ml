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

let test_eval ~source ~expected ~typ () =
  let out, out_typ = parse_expr source |> eval in
  Alcotest.check Testable.value "evaluate as expected" expected out;
  Alcotest.check Testable.type_ "typecheck as expected" typ out_typ

let higher_order =
  Alcotest.
    [
      test_case "apply twice" `Quick
        (test_eval
           ~source:
             "let twice = fun f -> fun x -> f (f x) in let inc = fun n -> n + \
              1 in twice inc 3"
           ~expected:(Interp.Model.Value.Atom (Syntax.Ast.Atom.I64 5L))
           ~typ:Type.Generic.i64);
    ]

let recursion =
  Alcotest.
    [
      test_case "fact 5" `Quick
        (test_eval
           ~source:
             "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) \
              in fact 5"
           ~expected:(Interp.Model.Value.Atom (Syntax.Ast.Atom.I64 120L))
           ~typ:Type.Generic.i64);
      test_case "fib 6" `Quick
        (test_eval
           ~source:
             "let rec fib = fun n -> if n <= 1 then n else fib (n-1) + fib \
              (n-2) in fib 6"
           ~expected:(Interp.Model.Value.Atom (Syntax.Ast.Atom.I64 8L))
           ~typ:Type.Generic.i64);
    ]

let lists =
  Alcotest.
    [
      (let rec odds n =
         if n > 9L then Interp.Model.Value.Tagged { tag = "Nil"; inner = None }
         else
           Interp.Model.Value.Tagged
             {
               tag = "Cons";
               inner = Some (Prod (Atom (I64 n), odds Int64.(add n 2L)));
             }
       in
       test_case "odd list 1..9 (recursive)" `Quick
         (test_eval
            ~source:
              {|
type List = Nil | Cons I64 * List in
let rec odds =
 fun n ->
   if n > 9 then Nil
   else Cons(n, odds (n + 2))
in odds 1
|}
            ~expected:(odds 1L)
            ~typ:Type.Generic.(con_0 "List")));
    ]

let () =
  Alcotest.run "Interp"
    [
      ("higher_order", higher_order); ("recursion", recursion); ("lists", lists);
    ]
