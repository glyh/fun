module Testable = struct
  let type_ =
    let pp_binding ppf ty = Fmt.pf ppf "%S" (Type.T.pp ty) in
    Alcotest.testable pp_binding Type.T.equal
end

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.expr_eof lexer

let test_typecheck ?(tag = "same type") ~source ~expected () =
  let expr = parse_expr source in
  prerr_endline (Syntax.Ast.Expr.pp expr);
  let inferred_type =
    Typecheck.Inference.on_expr Typecheck.TypeEnv.default expr
  in
  Printf.printf "%s vs %s\n" (Type.T.pp inferred_type) (Type.T.pp expected);
  Alcotest.(check Testable.type_) tag expected inferred_type

let constants =
  Alcotest.
    [
      test_case "single int" `Quick
        (test_typecheck ~source:"42" ~expected:Type.Generic.i64);
      test_case "unit literal" `Quick
        (test_typecheck ~source:"()" ~expected:Type.Generic.unit);
      test_case "true literal" `Quick
        (test_typecheck ~source:"true" ~expected:Type.Generic.bool);
      test_case "false literal" `Quick
        (test_typecheck ~source:"false" ~expected:Type.Generic.bool);
    ]

let let_bindings =
  Alcotest.
    [
      test_case "simple let" `Quick
        (test_typecheck ~source:"let x = 1 in x" ~expected:Type.Generic.i64);
      test_case "let bool" `Quick
        (test_typecheck ~source:"let b = true in b" ~expected:Type.Generic.bool);
      test_case "let shadowing" `Quick
        (test_typecheck ~source:"let x = true in let x = 1 in x"
           ~expected:Type.Generic.i64);
      test_case "simple recursive function" `Quick
        (test_typecheck
           ~source:
             "let rec f = fun x -> if x == 0 then 1 else x * f (x - 1) in f"
           ~expected:Type.Generic.(Arrow (i64, i64)));
      test_case "recursive call inside body" `Quick
        (test_typecheck
           ~source:
             "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) \
              in fact 5"
           ~expected:Type.Generic.i64);
    ]

let conditionals =
  Alcotest.
    [
      test_case "simple if" `Quick
        (test_typecheck ~source:"if true then 1 else 2"
           ~expected:Type.Generic.i64);
      test_case "if branches must match" `Quick (fun () ->
          Alcotest.check_raises "type mismatched in AST"
            Typecheck.(
              Exceptions.UnificationFailure
                Type.Generic.(Constraint.{ lhs = i64; rhs = bool }))
            (fun () ->
              parse_expr "if true then 1 else false"
              |> Typecheck.Inference.on_expr Typecheck.TypeEnv.default
              |> ignore));
      test_case "if nested" `Quick
        (test_typecheck ~source:"if false then (if true then 1 else 2) else 3"
           ~expected:Type.Generic.i64);
    ]

let lambdas =
  Alcotest.
    [
      test_case "lambda application" `Quick
        (test_typecheck ~source:"(fun x -> x) 3" ~expected:Type.Generic.i64);
      test_case "identity lambda" `Quick
        (test_typecheck ~source:"fun x -> x"
           ~expected:
             (Type.T.of_human (Forall ([ "x" ], Arrow (Var "x", Var "x")))));
      test_case "lambda returning bool" `Quick
        (test_typecheck ~source:"fun x -> true"
           ~expected:
             (Type.T.of_human
                (Forall ([ "x" ], Arrow (Var "x", Con ("Bool", []))))));
      test_case "apply bool function" `Quick
        (test_typecheck ~source:"(fun b -> if b then 1 else 0) false"
           ~expected:Type.Generic.i64);
    ]

let annotations =
  Alcotest.
    [
      (* === Literals and simple expressions === *)
      test_case "int annotation" `Quick
        (test_typecheck ~source:"(1 : I64)" ~expected:Type.Generic.i64);
      test_case "bool annotation" `Quick
        (test_typecheck ~source:"(true : Bool)" ~expected:Type.Generic.bool);
      test_case "redundant annotation" `Quick
        (test_typecheck ~source:"((1 + 1) : I64)" ~expected:Type.Generic.i64);
      (* === Let bindings === *)
      test_case "let with annotation" `Quick
        (test_typecheck ~source:"let x : I64 = 1 in x + 2"
           ~expected:Type.Generic.i64);
      (* test_case "let binding with polymorphic annotation" `Quick *)
      (*   (test_typecheck *)
      (*      ~source:"let id : forall 'a. 'a -> 'a = fun x -> x in id 10" *)
      (*      ~expected:Type.Generic.i64); *)
      (* test_case "let generalization check" `Quick *)
      (*   (test_typecheck *)
      (*      ~source:"let id = (fun x -> x : forall 'a. 'a -> 'a) in id true" *)
      (*      ~expected:Type.Generic.bool); *)
      test_case "let annotation affecting inference" `Quick
        (test_typecheck ~source:"let f : I64 -> I64 = fun x -> x + 1 in f 10"
           ~expected:Type.Generic.i64);
      test_case "nested annotated expression" `Quick
        (test_typecheck ~source:"let x = ((fun y -> y) : I64 -> I64) in x 3"
           ~expected:Type.Generic.i64);
      (* === Conditionals === *)
      test_case "if with annotated branch" `Quick
        (test_typecheck ~source:"if true then (1 : I64) else (2 : I64)"
           ~expected:Type.Generic.i64);
      (* === Mixed inference and annotation === *)
      (* test_case "polymorphic annotation at call site" `Quick *)
      (*   (test_typecheck *)
      (*      ~source:"let id = fun x -> x in (id : forall 'a. 'a -> 'a) false" *)
      (*      ~expected:Type.Generic.bool); *)
      test_case "mismatched but coerced via annotation" `Quick
        (test_typecheck ~source:"(fun x -> x + 1 : I64 -> I64)"
           ~expected:
             (Type.T.of_human (Arrow (Con ("I64", []), Con ("I64", [])))));
    ]

let adts =
  Alcotest.
    [
      test_case "constructor application is monomorphic" `Quick
        (test_typecheck ~source:"type option['a] = Some 'a | None in Some 1"
           ~expected:Type.Generic.(Con ("option", [ i64 ])));
      test_case "constructor polymorphism" `Quick
        (test_typecheck
           ~source:
             "type option['a] = Some 'a | None in let x = Some 1 in let y = \
              Some true in y"
           ~expected:Type.Generic.(Con ("option", [ bool ])));
      test_case "result Ok" `Quick
        (test_typecheck ~source:" type result['a, 'b] = Ok 'a | Err 'b in Ok 1"
           ~expected:
             (Type.T.of_human
                Type.Generic.(
                  Forall ([ "a" ], Con ("result", [ con_0 "I64"; Var "a" ])))));
      test_case "nullary constructor" `Quick
        (test_typecheck
           ~source:"type option['a] = Some 'a | None in None\n     "
           ~expected:
             (Type.T.of_human (Forall ([ "a" ], Con ("option", [ Var "a" ])))));
      test_case "if with ADT branches" `Quick
        (test_typecheck
           ~source:
             "type result['a, 'b] = Ok 'a | Err 'b in if true then Ok 1 else \
              Err ()"
           ~expected:Type.Generic.(Con ("result", [ i64; unit ])));
      test_case "let-bound constructor result is generalized" `Quick
        (test_typecheck
           ~source:
             "type option['a] = Some 'a | None in let x = Some 1 in x\n     "
           ~expected:Type.Generic.(Con ("option", [ i64 ])));
    ]

let () =
  Alcotest.run "Typecheck"
    [
      ("constants", constants);
      ("let_bindings", let_bindings);
      ("conditionals", conditionals);
      ("lambdas", lambdas);
      ("annotations", annotations);
      ("adts", adts);
    ]
