open Surface

let parse_ok source () =
  ignore (Core_lexer.parse_expr source)

let parse_fail source () =
  match Core_lexer.parse_expr source with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected parse failure"

let dotted_field_shape () =
  match Core_lexer.parse_expr "State.get" with
  | FieldAccess (Var "State", "get") -> ()
  | _ -> Alcotest.fail "expected ordinary field access"

let module_signature_sugar_shape () =
  match Core_lexer.parse_expr "sig x : I64; y : Bool end" with
  | Module { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = true }; LetBinding { name = "y"; value = Var "Bool"; public = true } ] } -> ()
  | _ -> Alcotest.fail "expected signature sugar as public type module"

let module_signature_param_shape () =
  match Core_lexer.parse_expr "fun (m : module let x = I64 end) -> m.x" with
  | Lam ({ name = "m"; type_ = Some (Module { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = false } ] }); _ }, FieldAccess (Var "m", "x")) -> ()
  | _ -> Alcotest.fail "expected module signature parameter"

let neq_still_parses_shape () =
  match Core_lexer.parse_expr "1 != 2" with
  | Ap (Ap (Var "!=", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected inequality"

let suites =
  [
    ( "parse_smoke",
      [
        Alcotest.test_case "match" `Quick (parse_ok "match 1 with x -> 2 end");
        Alcotest.test_case "adt match" `Quick (parse_ok "type Color = Red in match Red with x -> 1 end");
        Alcotest.test_case "dotted field shape" `Quick dotted_field_shape;
        Alcotest.test_case "module signature sugar shape" `Quick module_signature_sugar_shape;
        Alcotest.test_case "module signature parameter shape" `Quick module_signature_param_shape;
        Alcotest.test_case "inequality still parses" `Quick neq_still_parses_shape;
        Alcotest.test_case "resume without argument rejected" `Quick (parse_fail "resume");
      ] );
  ]
