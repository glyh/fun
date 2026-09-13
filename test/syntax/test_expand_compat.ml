(* These previously round-tripped a lowered [Syntax.t] back through
   [Surface_to_syntax] and compared with [Alcotest.pass] - a testable that always
   succeeds, so the comparison asserted nothing. (Verified: substituting an
   unrelated parse still passed.) The reverse conversion had no production caller
   and has been deleted; what these cases actually provided was "this source
   parses and lowers without raising", which is now what they say. *)
let check_compat source () =
  match Shape.lower_expr (Parse_expand.parse_expr source) with
  | _ -> ()

let token_spans () =
  match Raw_syntax.raw_tokens_with_spans "do x = 42; x end" with
  | [ { kind = Raw_syntax.KwDo; span = do_span };
      { kind = Raw_syntax.Ident "x"; span = x_span };
      { kind = Raw_syntax.Equals; _ };
      { kind = Raw_syntax.Int 42L; span = int_span };
      { kind = Raw_syntax.Semi; _ };
      { kind = Raw_syntax.Ident "x"; _ };
      { kind = Raw_syntax.KwEnd; _ };
      { kind = Raw_syntax.Eof; _ } ] ->
    Alcotest.(check int) "do start" 0 do_span.start_byte;
    Alcotest.(check int) "do end" 2 do_span.end_byte;
    Alcotest.(check int) "x start" 3 x_span.start_byte;
    Alcotest.(check int) "int start" 7 int_span.start_byte
  | _ -> Alcotest.fail "unexpected token stream"

let module_compat () =
  match Shape.lower_expr (Parse_expand.parse_module "pub x = 1; pub type Option A = Some A | None") with
  | _ -> ()

let nested_same_name_lets_preserve_resolved_identity () =
  match Shape.lower_expr (Parse_expand.parse_expr "do x = 1; x = 2; x end") with
  | Shape.Let { name = outer; body = Shape.Let { name = inner; body = Shape.Var use; _ }; _ } ->
      Alcotest.(check bool) "shadow gets distinct lowered name" true (not (String.equal outer inner));
      Alcotest.(check string) "use resolves to inner" inner use
  | _ -> Alcotest.fail "expected nested let shape"

let lambda_parameter_shadows_outer_let () =
  match Shape.lower_expr (Parse_expand.parse_expr "do x = 1; (fn(x) -> x : I64 -> I64) end") with
  | Shape.Let { name = outer; body = Shape.Annotated { inner = Shape.Lam ({ name = inner; _ }, Shape.Var use); _ }; _ } ->
      Alcotest.(check bool) "lambda parameter gets distinct lowered name" true (not (String.equal outer inner));
      Alcotest.(check string) "use resolves to parameter" inner use
  | _ -> Alcotest.fail "expected lambda shadowing shape"

let nonrec_let_rhs_does_not_see_own_binding () =
  match Shape.lower_expr (Parse_expand.parse_expr "do x = 1; x = x; x end") with
  | Shape.Let { name = outer; body = Shape.Let { name = inner; value = Shape.Var rhs; body = Shape.Var use; _ }; _ } ->
      Alcotest.(check bool) "inner lowered name is distinct" true (not (String.equal outer inner));
      Alcotest.(check string) "rhs resolves to outer" outer rhs;
      Alcotest.(check string) "body resolves to inner" inner use
  | _ -> Alcotest.fail "expected non-rec let shape"

let pattern_binder_shadows_outer_only_in_branch () =
  match Shape.lower_expr (Parse_expand.parse_expr "do x = 10; match 1 do x -> x end end") with
  | Shape.Let { name = outer; body = Shape.Match (_, [ Shape.ValueBranch (Shape.PatBind inner, Shape.Var use) ]); _ } ->
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
        Alcotest.test_case "application" `Quick (check_compat "f(x)");
        Alcotest.test_case "lambda" `Quick (check_compat "fn(x) -> x");
        Alcotest.test_case "let" `Quick (check_compat "do x = 1; x end");
        Alcotest.test_case "annotation" `Quick (check_compat "(1 : I64)");
        Alcotest.test_case "field access" `Quick (check_compat "p.x");
        Alcotest.test_case "record construction" `Quick (check_compat "Point{x = 1; y = 2}");
        Alcotest.test_case "match" `Quick (check_compat "match x do Some(y) -> y | None -> 0 end");
        Alcotest.test_case "struct" `Quick (check_compat "struct x: I64; pub y = 1 end");
        Alcotest.test_case "module" `Quick (check_compat "module pub x = 1 end");
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
