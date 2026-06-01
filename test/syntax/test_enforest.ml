open Surface

let parse = Parse_expand.parse_expr

let raw_grouping () =
  match Raw_syntax.read "(a, [b], {c})" with
  | [ { datum = Group (Paren, items, _); _ } ] ->
      let has_bracket =
        List.exists
          (function { Raw_syntax.datum = Group (Bracket, _, _); _ } -> true | _ -> false)
          items
      in
      let has_brace =
        List.exists
          (function { Raw_syntax.datum = Group (Brace, _, _); _ } -> true | _ -> false)
          items
      in
      if not (has_bracket && has_brace) then Alcotest.fail "expected nested bracket and brace groups"
  | _ -> Alcotest.fail "expected one parenthesized group"

let line_and_block_comments () =
  match parse "# leading\n1 #| nested #| block |# comment |# + # inline\n2" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected comments to be skipped"

let datum_comment () =
  match parse "#_ skipped 1 + 2" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected datum comment to skip one term"

let operator_precedence () =
  match parse "1 + 2 * 3" with
  | Ap
      ( Ap (Var "+", Explicit, Atom (Atom.I64 1L)),
        Explicit,
        Ap (Ap (Var "*", Explicit, Atom (Atom.I64 2L)), Explicit, Atom (Atom.I64 3L)) ) ->
      ()
  | _ -> Alcotest.fail "expected multiplication to bind tighter than addition"

let left_associativity () =
  match parse "1 - 2 - 3" with
  | Ap
      ( Ap
          ( Var "-",
            Explicit,
            Ap (Ap (Var "-", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) ),
        Explicit,
        Atom (Atom.I64 3L) ) ->
      ()
  | _ -> Alcotest.fail "expected left associative subtraction"

let prefix_not () =
  match parse "not true" with
  | Ap (Var "not", Explicit, Atom (Atom.Bool true)) -> ()
  | _ -> Alcotest.fail "expected prefix not application"

let ref_assignment () =
  match parse "r <- 2" with
  | RefSet (Var "r", Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected <- to lower to RefSet"

let field_access () =
  match parse "m.x.y" with
  | FieldAccess (FieldAccess (Var "m", "x"), "y") -> ()
  | _ -> Alcotest.fail "expected chained field access"

let curried_call () =
  match parse "f(1, 2, 3)" with
  | Ap
      ( Ap (Ap (Var "f", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)),
        Explicit,
        Atom (Atom.I64 3L) ) ->
      ()
  | _ -> Alcotest.fail "expected curried call lowering"

let zero_argument_call () =
  match parse "f()" with
  | Ap (Var "f", Explicit, Atom Atom.Unit) -> ()
  | _ -> Alcotest.fail "expected zero-argument call to pass Unit"

let implicit_call () =
  match parse "id[I64](1)" with
  | Ap (Ap (Var "id", Implicit, Var "I64"), Explicit, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected bracket call to lower to implicit application"

let do_block_bindings () =
  match parse "do\n  x = 1\n  y = x + 1\n  y\nend" with
  | Let
      {
        name = "x";
        value = Atom (Atom.I64 1L);
        body =
          Let
            {
              name = "y";
              value = Ap (Ap (Var "+", Explicit, Var "x"), Explicit, Atom (Atom.I64 1L));
              body = Var "y";
              _;
            };
        _;
      } ->
      ()
  | _ -> Alcotest.fail "expected do block to lower to nested lets"

let old_syntax_fallback () =
  match parse "let x = 1 in x" with
  | Let { name = "x"; value = Atom (Atom.I64 1L); body = Var "x"; _ } -> ()
  | _ -> Alcotest.fail "expected old let syntax to keep parsing through fallback"

let malformed_do_rejected () =
  match parse "do x = end" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected malformed do block to be rejected"

let suites =
  [ ( "enforest",
      [ Alcotest.test_case "raw grouping" `Quick raw_grouping;
        Alcotest.test_case "comments" `Quick line_and_block_comments;
        Alcotest.test_case "datum comment" `Quick datum_comment;
        Alcotest.test_case "operator precedence" `Quick operator_precedence;
        Alcotest.test_case "left associativity" `Quick left_associativity;
        Alcotest.test_case "prefix not" `Quick prefix_not;
        Alcotest.test_case "ref assignment" `Quick ref_assignment;
        Alcotest.test_case "field access" `Quick field_access;
        Alcotest.test_case "curried call" `Quick curried_call;
        Alcotest.test_case "zero argument call" `Quick zero_argument_call;
        Alcotest.test_case "implicit call" `Quick implicit_call;
        Alcotest.test_case "do block bindings" `Quick do_block_bindings;
        Alcotest.test_case "old syntax fallback" `Quick old_syntax_fallback;
        Alcotest.test_case "malformed do rejected" `Quick malformed_do_rejected;
      ] );
  ]
