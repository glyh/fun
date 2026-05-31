let expand_lower surface =
  surface
  |> Surface_to_syntax.expr
  |> Expand.expand_expr
  |> Lower_surface.lower_expr

let check_compat source () =
  let parsed = Parse_expand.parse_expr source in
  Alcotest.(check pass) source parsed (expand_lower parsed)

let token_spans () =
  match Core_lexer.tokens_with_spans "let x = 42 in x" with
  | [ { token = Core_parser.LET; span = let_span };
      { token = Core_parser.ID "x"; span = x_span };
      { token = Core_parser.EQUALS; _ };
      { token = Core_parser.INT 42L; span = int_span };
      { token = Core_parser.IN; _ };
      { token = Core_parser.ID "x"; _ };
      { token = Core_parser.EOF; _ } ] ->
    Alcotest.(check int) "let start" 0 let_span.start_byte;
    Alcotest.(check int) "let end" 3 let_span.end_byte;
    Alcotest.(check int) "x start" 4 x_span.start_byte;
    Alcotest.(check int) "int start" 8 int_span.start_byte
  | _ -> Alcotest.fail "unexpected token stream"

let module_compat () =
  let parsed = Parse_expand.parse_module "pub let x = 1; pub type Option A = Some A | None" in
  Alcotest.(check pass) "module compat" parsed (expand_lower parsed)

let suites =
  [
    ( "tokens",
      [ Alcotest.test_case "tokens preserve spans" `Quick token_spans ] );
    ( "expand_compat_expr",
      [
        Alcotest.test_case "variable" `Quick (check_compat "x");
        Alcotest.test_case "atom" `Quick (check_compat "42");
        Alcotest.test_case "application" `Quick (check_compat "f x");
        Alcotest.test_case "lambda" `Quick (check_compat "fun x -> x");
        Alcotest.test_case "let" `Quick (check_compat "let x = 1 in x");
        Alcotest.test_case "annotation" `Quick (check_compat "(1 : I64)");
        Alcotest.test_case "field access" `Quick (check_compat "p.x");
        Alcotest.test_case "record construction" `Quick (check_compat "Point {x = 1; y = 2}");
        Alcotest.test_case "match" `Quick (check_compat "match x with Some y -> y | None -> 0 end");
        Alcotest.test_case "struct" `Quick (check_compat "struct x: I64; pub let y = 1 end");
        Alcotest.test_case "module" `Quick (check_compat "module pub let x = 1 end");
      ] );
    ( "expand_compat_module",
      [ Alcotest.test_case "module body" `Quick module_compat ] );
  ]
