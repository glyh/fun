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

let nested_same_name_lets_preserve_resolved_identity () =
  match Parse_expand.parse_expr "let x = 1 in let x = 2 in x" with
  | Surface.Let { name = outer; body = Surface.Let { name = inner; body = Surface.Var use; _ }; _ } ->
      Alcotest.(check bool) "shadow gets distinct lowered name" true (not (String.equal outer inner));
      Alcotest.(check string) "use resolves to inner" inner use
  | _ -> Alcotest.fail "expected nested let shape"

let lambda_parameter_shadows_outer_let () =
  match Parse_expand.parse_expr "let x = 1 in (fun x -> x : I64 -> I64)" with
  | Surface.Let { name = outer; body = Surface.Annotated { inner = Surface.Lam ({ name = inner; _ }, Surface.Var use); _ }; _ } ->
      Alcotest.(check bool) "lambda parameter gets distinct lowered name" true (not (String.equal outer inner));
      Alcotest.(check string) "use resolves to parameter" inner use
  | _ -> Alcotest.fail "expected lambda shadowing shape"

let nonrec_let_rhs_does_not_see_own_binding () =
  match Parse_expand.parse_expr "let x = 1 in let x = x in x" with
  | Surface.Let { name = outer; body = Surface.Let { name = inner; value = Surface.Var rhs; body = Surface.Var use; _ }; _ } ->
      Alcotest.(check bool) "inner lowered name is distinct" true (not (String.equal outer inner));
      Alcotest.(check string) "rhs resolves to outer" outer rhs;
      Alcotest.(check string) "body resolves to inner" inner use
  | _ -> Alcotest.fail "expected non-rec let shape"

let pattern_binder_shadows_outer_only_in_branch () =
  match Parse_expand.parse_expr "let x = 10 in match 1 with x -> x end" with
  | Surface.Let { name = outer; body = Surface.Match (_, [ Surface.ValueBranch (Surface.PatBind inner, Surface.Var use) ]); _ } ->
      Alcotest.(check bool) "pattern binder gets distinct lowered name" true (not (String.equal outer inner));
      Alcotest.(check string) "branch body resolves to pattern binder" inner use
  | _ -> Alcotest.fail "expected pattern shadowing shape"

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
    ( "expand_hygiene",
      [
        Alcotest.test_case "nested same-name lets preserve identity" `Quick nested_same_name_lets_preserve_resolved_identity;
        Alcotest.test_case "lambda parameter shadows outer let" `Quick lambda_parameter_shadows_outer_let;
        Alcotest.test_case "non-rec let rhs does not see own binding" `Quick nonrec_let_rhs_does_not_see_own_binding;
        Alcotest.test_case "pattern binder shadows only in branch" `Quick pattern_binder_shadows_outer_only_in_branch;
      ] );
  ]
