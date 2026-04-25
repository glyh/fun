open Interp.Model

module Testable = struct
  let type_ =
    let pp_type ppf ty = Fmt.pf ppf "%S" (Type.T.pp ty) in
    Alcotest.testable pp_type Type.T.equal

  let value =
    let pp_value ppf ty = Fmt.pf ppf "%S" (Value.pp ty) in
    Alcotest.testable pp_value Value.equal
end

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof lexer

let eval expr =
  let typed = Typecheck.Inference.on_expr Typecheck.TypeEnv.default expr in
  let result = Interp.(Eval.eval Env.default typed) in
  (result, typed.Typed_ir.Expr.type_)

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
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 5L)) ~typ:Type.Generic.i64);
    ]

let recursion =
  Alcotest.
    [
      test_case "fact 5" `Quick
        (test_eval
           ~source:
             "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) \
              in fact 5"
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 120L))
           ~typ:Type.Generic.i64);
      test_case "fib 6" `Quick
        (test_eval
           ~source:
             "let rec fib = fun n -> if n <= 1 then n else fib (n-1) + fib \
              (n-2) in fib 6"
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 8L)) ~typ:Type.Generic.i64);
    ]

let lists =
  Alcotest.
    [
      (let rec odds n =
         if n > 9L then Value.Tagged { tag = "Nil"; inner = None }
         else
           Value.Tagged
             {
               tag = "Cons";
               inner =
                 Some
                   (Prod
                      (Std.Nonempty_list.init (Value.Atom (I64 n))
                         [ odds Int64.(add n 2L) ]));
             }
       in
       test_case "odd list 1..9 (recursive)" `Quick
         (test_eval
            ~source:
              {|
type List = Nil | Cons (I64, List) in
let rec odds =
 fun n ->
   if n > 9 then Nil
   else Cons(n, odds (n + 2))
in odds 1
|}
            ~expected:(odds 1L)
            ~typ:Type.Generic.(con_0 "List")));
    ]

let matches =
  Alcotest.
    [
      test_case "literal switch" `Quick
        (test_eval
           ~source:
             {|match 1
| 0 -> 10
| 1 -> 11
| _ -> 0
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 11L))
           ~typ:Type.Generic.i64);
      test_case "tuple projection" `Quick
        (test_eval
           ~source:
             {|match (1, 2)
| (x, y) -> x + y
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 3L))
           ~typ:Type.Generic.i64);
      test_case "tagged constructor with payload" `Quick
        (test_eval
           ~source:
             {|type wrapper = W I64 in
match W 41
| W(x) -> x + 1
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "union pattern dispatch" `Quick
        (test_eval
           ~source:
             {|match 1
| 0 | 1 -> 42
| _ -> 0
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "first branch wins" `Quick
        (test_eval
           ~source:
             {|match 1
| _ -> 0
| 1 -> 1
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 0L))
           ~typ:Type.Generic.i64);
    ]

let test_non_exhaustive ~source ~expected () =
  let missing_pat =
    let pp ppf p = Fmt.pf ppf "%s" (Match.Missing_pat.pp p) in
    Alcotest.testable pp Match.Missing_pat.equal
  in
  match parse_expr source |> eval with
  | _ -> Alcotest.fail "expected Non_exhaustive but evaluation succeeded"
  | exception Match.Match_compile.Non_exhaustive actual ->
      Alcotest.check missing_pat "missing pattern" expected actual

let exhaustiveness =
  let open Match.Missing_pat in
  let nel x xs = Std.Nonempty_list.init x xs in
  let many xs = Many (nel (List.hd xs) (List.tl xs)) in
  Alcotest.
    [
      test_case "exhaustiveness: deeply nested" `Quick
        (test_non_exhaustive
           ~source:
             {|type c = X I64 | Y I64 in
type b = P c | Q c in
type a = M b | N b in
match M (P (X 1))
| M(P(X(x))) -> x
end|}
           ~expected:
             (many
                [ ( "M",
                    Some
                      (many
                         [ ("P", Some (many [ ("Y", Some Wildcard) ]));
                           ("Q", Some Wildcard) ]) );
                  ("N", Some Wildcard) ]));
      test_case "exhaustiveness: multiple branches partially covered" `Quick
        (test_non_exhaustive
           ~source:
             {|type inner = X I64 | Y I64 in
type outer = A inner | B inner in
match A (X 1)
| A(X(x)) -> x
| B(X(x)) -> x
end|}
           ~expected:
             (many
                [ ("B", Some (many [ ("Y", Some Wildcard) ]));
                  ("A", Some (many [ ("Y", Some Wildcard) ])) ]));
      test_case "exhaustiveness: partial with three constructors" `Quick
        (test_non_exhaustive
           ~source:
             {|type abc = A I64 | B I64 | C I64 in
match A 1
| A(x) -> x
| B(x) -> x
end|}
           ~expected:(many [ ("C", Some Wildcard) ]));
      test_case "exhaustiveness: union pattern" `Quick
        (test_non_exhaustive
           ~source:
             {|type abc = A I64 | B I64 | C I64 in
match A 1
| A(x) | B(x) -> x
end|}
           ~expected:(many [ ("C", Some Wildcard) ]));
    ]

let () =
  Alcotest.run "Interp"
    [
      ("higher_order", higher_order);
      ("recursion", recursion);
      ("lists", lists);
      ("matches", matches @ exhaustiveness);
    ]
