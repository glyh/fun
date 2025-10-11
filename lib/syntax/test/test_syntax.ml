open Syntax.Ast

module Testable = struct
  let binding =
    let pp_binding ppf expr = Fmt.pf ppf "%S" (Binding.pp expr) in
    Alcotest.testable pp_binding Binding.equal
end

let parse_bindings s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.toplevel_eof
    lexer

let test_syntax ?(tag = "same AST") ~source ~expected () =
  let parsed = parse_bindings source in
  Alcotest.(check (list Testable.binding)) tag parsed expected

let commments =
  Alcotest.
    [
      test_case "single comment" `Quick
        (test_syntax ~source:"let x = 1 (* comment *)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 1L);
               };
             ]);
      test_case "nested comment" `Quick
        (test_syntax ~source:"let x = 1 (* outer (* inner *) outer *)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 1L);
               };
             ]);
      test_case "nested comment between tokens" `Quick
        (test_syntax ~source:"let (* a *) x (* b (* c *) d *) = (* e *) 1"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 1L);
               };
             ]);
    ]

let let_bindings =
  Alcotest.
    [
      test_case "simple let binding" `Quick
        (test_syntax ~source:"let x = 42"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 42L);
               };
             ]);
      test_case "multiple let bindings" `Quick
        (test_syntax ~source:"let x = 1;; let y = 2"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 1L);
               };
               {
                 recursive = false;
                 name = "y";
                 type_ = None;
                 value = Atom (I64 2L);
               };
             ]);
      test_case "multiple function bindings" `Quick
        (test_syntax ~source:"let f x = x;; let g y = y"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value = Lam ({ name = "x"; type_ = None }, Var "x");
               };
               {
                 recursive = false;
                 name = "g";
                 type_ = None;
                 value = Lam ({ name = "y"; type_ = None }, Var "y");
               };
             ]);
      test_case "nested let inside function body" `Quick
        (test_syntax ~source:"let f x = let y = 1 in y"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value =
                   Lam
                     ( { name = "x"; type_ = None },
                       Let
                         {
                           binding =
                             {
                               recursive = false;
                               name = "y";
                               type_ = None;
                               value = Atom (I64 1L);
                             };
                           body = Var "y";
                         } );
               };
             ]);
      test_case "recursive function with a mix of typed & untyped params" `Quick
        (test_syntax ~source:"let rec f (x: Int) y = x"
           ~expected:
             [
               {
                 recursive = true;
                 name = "f";
                 type_ = None;
                 value =
                   Lam
                     ( { name = "x"; type_ = Some (Con "Int") },
                       Lam ({ name = "y"; type_ = None }, Var "x") );
               };
             ]);
      test_case "recursive function (factorial)" `Quick
        (test_syntax
           ~source:
             "let rec fact = fun n -> if eq n 0 then 1 else mul n (fact (sub n \
              1))"
           ~expected:
             [
               {
                 recursive = true;
                 name = "fact";
                 type_ = None;
                 value =
                   Lam
                     ( { name = "n"; type_ = None },
                       If
                         {
                           cond = Ap (Ap (Var "eq", Var "n"), Atom (I64 0L));
                           then_ = Atom (I64 1L);
                           else_ =
                             Ap
                               ( Ap (Var "mul", Var "n"),
                                 Ap
                                   ( Var "fact",
                                     Ap (Ap (Var "sub", Var "n"), Atom (I64 1L))
                                   ) );
                         } );
               };
             ]);
    ]

let applications =
  Alcotest.
    [
      test_case "application left associativity" `Quick
        (test_syntax ~source:"let x = a b c"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Ap (Ap (Var "a", Var "b"), Var "c");
               };
             ]);
      test_case "application with parentheses on left" `Quick
        (test_syntax ~source:"let x = (f y) z"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Ap (Ap (Var "f", Var "y"), Var "z");
               };
             ]);
      test_case "application with parentheses on right" `Quick
        (test_syntax ~source:"let x = f (y z)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Ap (Var "f", Ap (Var "y", Var "z"));
               };
             ]);
      test_case "application of function call result" `Quick
        (test_syntax ~source:"let x = (f x) (g y)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Ap (Ap (Var "f", Var "x"), Ap (Var "g", Var "y"));
               };
             ]);
    ]

let ifs =
  Alcotest.
    [
      test_case "if expression with parentheses" `Quick
        (test_syntax ~source:"let x = if 1 then 2 else 3"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value =
                   If
                     {
                       cond = Atom (I64 1L);
                       then_ = Atom (I64 2L);
                       else_ = Atom (I64 3L);
                     };
               };
             ]);
      test_case "nested if in else branch" `Quick
        (test_syntax ~source:"let x = if 1 then 2 else if 3 then 4 else 5"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value =
                   If
                     {
                       cond = Atom (I64 1L);
                       then_ = Atom (I64 2L);
                       else_ =
                         If
                           {
                             cond = Atom (I64 3L);
                             then_ = Atom (I64 4L);
                             else_ = Atom (I64 5L);
                           };
                     };
               };
             ]);
    ]

let lambdas =
  Alcotest.
    [
      test_case "nested lambdas" `Quick
        (test_syntax ~source:"let f = fun x -> fun y -> x"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value =
                   Lam
                     ( { name = "x"; type_ = None },
                       Lam ({ name = "y"; type_ = None }, Var "x") );
               };
             ]);
      test_case "function returning function" `Quick
        (test_syntax ~source:"let f x = fun y -> y"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value =
                   Lam
                     ( { name = "x"; type_ = None },
                       Lam ({ name = "y"; type_ = None }, Var "y") );
               };
             ]);
    ]

let parentheses =
  Alcotest.
    [
      test_case "nested parentheses" `Quick
        (test_syntax ~source:"let x = (((42)))"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Atom (I64 42L);
               };
             ]);
    ]

let annotations =
  Alcotest.
    [
      test_case "simple annotation" `Quick
        (test_syntax ~source:"let x = (1 : int)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value = Annotated { inner = Atom (I64 1L); typ = Con "int" };
               };
             ]);
      test_case "annotation on variable" `Quick
        (test_syntax ~source:"let y = (x : bool)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "y";
                 type_ = None;
                 value = Annotated { inner = Var "x"; typ = Con "bool" };
               };
             ]);
      test_case "annotation on function application" `Quick
        (test_syntax ~source:"let f = ((g x) : t)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value =
                   Annotated { inner = Ap (Var "g", Var "x"); typ = Con "t" };
               };
             ]);
      test_case "annotation nested inside if" `Quick
        (test_syntax ~source:"let x = if cond then (y : t1) else (z : t2)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value =
                   If
                     {
                       cond = Var "cond";
                       then_ = Annotated { inner = Var "y"; typ = Con "t1" };
                       else_ = Annotated { inner = Var "z"; typ = Con "t2" };
                     };
               };
             ]);
      test_case "annotation around lambda" `Quick
        (test_syntax ~source:"let f = ((fun x -> x) : t -> t)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "f";
                 type_ = None;
                 value =
                   Annotated
                     {
                       inner = Lam ({ name = "x"; type_ = None }, Var "x");
                       typ = Arrow (Con "t", Con "t");
                     };
               };
             ]);
      test_case "annotation combined with application" `Quick
        (test_syntax ~source:"let x = (f : t1 -> t2) y"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value =
                   Ap
                     ( Annotated
                         { inner = Var "f"; typ = Arrow (Con "t1", Con "t2") },
                       Var "y" );
               };
             ]);
      test_case "nested annotation" `Quick
        (test_syntax ~source:"let x = ((1 : int) : num)"
           ~expected:
             [
               {
                 recursive = false;
                 name = "x";
                 type_ = None;
                 value =
                   Annotated
                     {
                       inner =
                         Annotated { inner = Atom (I64 1L); typ = Con "int" };
                       typ = Con "num";
                     };
               };
             ]);
    ]

let () =
  Alcotest.run "Syntax"
    [
      ("commments", commments);
      ("let_bindings", let_bindings);
      ("applications", applications);
      ("ifs", ifs);
      ("lambdas", lambdas);
      ("parentheses", parentheses);
      ("annotations", annotations);
    ]
