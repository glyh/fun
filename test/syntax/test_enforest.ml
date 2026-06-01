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

let fn_arrow_body () =
  match parse "fn (x : I64) -> x" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn arrow body to lower to lambda"

let fn_block_body () =
  match parse "fn (x : I64) do x end" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn block body to lower to lambda"

let fn_implicit_then_explicit_params () =
  match parse "fn [A : Type] (x : A) -> x" with
  | Lam
      ( { name = "A"; type_ = Some (Var "Type"); explicitness = Implicit; _ },
        Lam ({ name = "x"; type_ = Some (Var "A"); explicitness = Explicit; _ }, Var "x") ) ->
      ()
  | _ -> Alcotest.fail "expected implicit fn params before explicit params"

let fn_unit_param () =
  match parse "fn () -> 1" with
  | Lam ({ name = "_"; type_ = Some (Var "Unit"); explicitness = Explicit; _ }, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected fn () to lower to Unit parameter"

let fn_rejects_late_implicit_params () =
  match parse "fn (x : I64) [A : Type] -> x" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected implicit params after explicit params to be rejected"

let if_do_else_end () =
  match parse "if true do 1 else 2 end" with
  | If { cond = Atom (Atom.Bool true); then_ = Atom (Atom.I64 1L); else_ = Atom (Atom.I64 2L) } -> ()
  | _ -> Alcotest.fail "expected if do else end shape"

let match_do_end () =
  match parse "match true do | true -> 1 | false -> 0 end" with
  | Match
      ( Atom (Atom.Bool true),
        [ ValueBranch (PatAtom (Atom.Bool true), Atom (Atom.I64 1L));
          ValueBranch (PatAtom (Atom.Bool false), Atom (Atom.I64 0L)) ] ) ->
      ()
  | _ -> Alcotest.fail "expected match do end branches"

let ref_call_deref () =
  match parse "deref(ref(1))" with
  | RefGet (RefNew (Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected deref(ref(expr)) shape"

let deref_field_is_ordinary_access () =
  match parse "r.deref" with
  | FieldAccess (Var "r", "deref") -> ()
  | _ -> Alcotest.fail "expected .deref to remain ordinary field access"

let resume_unit_call () =
  match parse "resume()" with
  | Resume (Atom Atom.Unit) -> ()
  | _ -> Alcotest.fail "expected resume() to lower to Resume Unit"

let import_shape () =
  match parse "import \"x\"" with
  | Import "x" -> ()
  | _ -> Alcotest.fail "expected import string shape"

let open_in_do_block () =
  match parse "do open M; x end" with
  | Open ("M", Var "x") -> ()
  | _ -> Alcotest.fail "expected open statement in do block"

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
        Alcotest.test_case "fn arrow body" `Quick fn_arrow_body;
        Alcotest.test_case "fn block body" `Quick fn_block_body;
        Alcotest.test_case "fn implicit then explicit params" `Quick fn_implicit_then_explicit_params;
        Alcotest.test_case "fn unit param" `Quick fn_unit_param;
        Alcotest.test_case "fn rejects late implicit params" `Quick fn_rejects_late_implicit_params;
        Alcotest.test_case "if do else end" `Quick if_do_else_end;
        Alcotest.test_case "match do end" `Quick match_do_end;
        Alcotest.test_case "ref call deref" `Quick ref_call_deref;
        Alcotest.test_case "deref field is ordinary access" `Quick deref_field_is_ordinary_access;
        Alcotest.test_case "resume unit call" `Quick resume_unit_call;
        Alcotest.test_case "import shape" `Quick import_shape;
        Alcotest.test_case "open in do block" `Quick open_in_do_block;
      ] );
  ]
