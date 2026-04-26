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
  let typed, type_defs = Typecheck.Inference.on_expr Typecheck.TypeEnv.default expr in
  let result = Interp.(Eval.eval ~type_defs Env.default typed) in
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

let records =
  Alcotest.
    [
      test_case "record construction and field access" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
let p = {x = 10; y = 20} in p.x + p.y|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 30L))
           ~typ:Type.Generic.i64);
      test_case "record pattern matching" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
match {x = 3; y = 4}
| {x; y} -> x * y
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 12L))
           ~typ:Type.Generic.i64);
      test_case "record in ADT payload" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
type shape = Circle I64 | Rect point in
match Rect {x = 3; y = 4}
| Circle(r) -> r
| Rect(p) -> p.x + p.y
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 7L))
           ~typ:Type.Generic.i64);
      test_case "polymorphic record field access" `Quick
        (test_eval
           ~source:
             {|type pair['a, 'b] = {fst: 'a; snd: 'b} in
let p = {fst = 1; snd = true} in
if p.snd then p.fst else 0|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 1L))
           ~typ:Type.Generic.i64);
      test_case "polymorphic record at multiple instantiations" `Quick
        (test_eval
           ~source:
             {|type pair['a, 'b] = {fst: 'a; snd: 'b} in
let p1 = {fst = 10; snd = 20} in
let p2 = {fst = true; snd = 3} in
if p2.fst then p1.fst + p2.snd else 0|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 13L))
           ~typ:Type.Generic.i64);
      test_case "record field order doesn't matter in construction" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
let p = {y = 20; x = 10} in p.x + p.y|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 30L))
           ~typ:Type.Generic.i64);
      test_case "record field order doesn't matter in pattern" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
match {x = 3; y = 4}
| {y; x} -> x * y
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 12L))
           ~typ:Type.Generic.i64);
      test_case "record field order doesn't matter in equality" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
if {x = 1; y = 2} == {y = 2; x = 1} then 1 else 0|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 1L))
           ~typ:Type.Generic.i64);
      test_case "record pattern with rename and shorthand" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
match {x = 10; y = 20}
| {x = wow; y} -> wow + y
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 30L))
           ~typ:Type.Generic.i64);
      test_case "partial record pattern" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
match {x = 3; y = 4}
| {x; _} -> x
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 3L))
           ~typ:Type.Generic.i64);
      test_case "record pattern with literal in field" `Quick
        (test_eval
           ~source:
             {|type point = {x: I64; y: I64} in
match {x = 1; y = 99}
| {x = 0; y} -> y
| {x = 1; y} -> y + 1
| {x; y} -> 0
end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 100L))
           ~typ:Type.Generic.i64);
    ]

let structs =
  Alcotest.
    [
      test_case "struct with pub value, field access" `Quick
        (test_eval
           ~source:
             {|let M = struct pub let x = 42 end in M.x|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "struct private helper, pub uses it" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 let secret = 10
                 pub let x = secret + 1
               end in M.x|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 11L))
           ~typ:Type.Generic.i64);
      test_case "struct with pub function" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 let helper = fun x -> x * 2
                 pub let double = helper
               end in M.double 21|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "struct with multiple pub members" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 pub let a = 1
                 pub let b = 2
               end in M.a + M.b|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 3L))
           ~typ:Type.Generic.i64);
      test_case "empty struct" `Quick
        (test_eval
           ~source:
             {|let M = struct end in 42|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "struct with pub type, qualified constructor" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 pub type color = Red | Green | Blue
               end in
               match M.Red
               | M.Red -> 1
               | M.Green -> 2
               | M.Blue -> 3
               end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 1L))
           ~typ:Type.Generic.i64);
      test_case "struct with pub type, constructor with payload" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 pub type wrapper = W I64
               end in
               match M.W 42
               | M.W(x) -> x
               end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 42L))
           ~typ:Type.Generic.i64);
      test_case "nested struct, qualified constructor" `Quick
        (test_eval
           ~source:
             {|let A = struct
                 pub let B = struct
                   pub type t = X I64
                 end
               end in
               match A.B.X 7
               | A.B.X(n) -> n
               end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 7L))
           ~typ:Type.Generic.i64);
      test_case "struct alias, qualified constructor" `Quick
        (test_eval
           ~source:
             {|let M = struct
                 pub type color = Red | Green | Blue
               end in
               let N = M in
               match N.Red
               | N.Red -> 1
               | N.Green -> 2
               | N.Blue -> 3
               end|}
           ~expected:(Value.Atom (Syntax.Ast.Atom.I64 1L))
           ~typ:Type.Generic.i64);
    ]

let make_loader base_dir =
  Loader.create ~base_dir
    ~typecheck_fn:(fun ?loader expr ->
      Typecheck.Inference.on_expr ?loader Typecheck.TypeEnv.default expr)

let with_modules modules f =
  let dir = Filename.temp_dir "fun_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = make_loader dir in
  f ~loader

let eval_with_loader ~loader source =
  let expr = parse_expr source in
  let typed, type_defs =
    Typecheck.Inference.on_expr ~loader Typecheck.TypeEnv.default expr
  in
  let result = Interp.(Eval.eval ~type_defs Env.default typed) in
  (result, typed.Typed_ir.Expr.type_)

let imports =
  Alcotest.
    [
      test_case "basic import, pub value" `Quick (fun () ->
        with_modules
          [ ("math", "pub let double x = x + x") ]
          (fun ~loader ->
            let result, _typ = eval_with_loader ~loader
              {|let M = import "math" in M.double 5|}
            in
            Alcotest.check Testable.value "10" (Value.Atom (Syntax.Ast.Atom.I64 10L)) result));
      test_case "import with ADT, pattern match" `Quick (fun () ->
        with_modules
          [ ("color", "pub type color = Red | Green | Blue") ]
          (fun ~loader ->
            let result, _typ = eval_with_loader ~loader
              {|let C = import "color" in
                match C.Red
                | C.Red -> 1
                | C.Green -> 2
                | C.Blue -> 3
                end|}
            in
            Alcotest.check Testable.value "1" (Value.Atom (Syntax.Ast.Atom.I64 1L)) result));
      test_case "nested import" `Quick (fun () ->
        with_modules
          [ ("base", "pub let x = 42");
            ("wrapper", {|pub let M = import "base"|}) ]
          (fun ~loader ->
            let result, _typ = eval_with_loader ~loader
              {|let W = import "wrapper" in W.M.x|}
            in
            Alcotest.check Testable.value "42" (Value.Atom (Syntax.Ast.Atom.I64 42L)) result));
      test_case "circular import detected" `Quick (fun () ->
        with_modules
          [ ("a", {|pub let x = import "b"|});
            ("b", {|pub let y = import "a"|}) ]
          (fun ~loader ->
            Alcotest.check_raises "circular"
              (Loader.CircularImport "a")
              (fun () ->
                ignore (eval_with_loader ~loader {|import "a"|}))))
    ]

let () =
  Alcotest.run "Interp"
    [
      ("higher_order", higher_order);
      ("recursion", recursion);
      ("lists", lists);
      ("matches", matches @ exhaustiveness);
      ("records", records);
      ("structs", structs);
      ("imports", imports);
    ]
