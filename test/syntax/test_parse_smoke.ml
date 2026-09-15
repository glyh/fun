let unwrap_std (e : Shape.t) : Shape.t = match e with Shape.Open (Shape.Import "std", body, _) -> body | other -> other
let parse_expr source = unwrap_std (Parse_written.parse_expr ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source)
let parse_module source = Parse_written.parse_module ~load_syntax:Macro_driver.std_load_syntax source
open Shape

let parse_ok source () =
  ignore (parse_expr source)

let parse_fail source () =
  match parse_expr source with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected parse failure"

let dotted_field_shape () =
  match parse_expr "State.get" with
  | FieldAccess (Var "State", "get") -> ()
  | _ -> Alcotest.fail "expected ordinary field access"

let module_signature_sugar_shape () =
  match parse_expr "sig { x : I64; y : Bool }" with
  | Sig { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = true; _ }; LetBinding { name = "y"; value = Var "Bool"; public = true; _ } ] } -> ()
  | _ -> Alcotest.fail "expected a signature value"

let module_signature_param_shape () =
  match parse_expr "fn(m : sig { x : I64 }) { m.x }" with
  | Lam ({ name = "m"; type_ = Some (Sig { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = true; _ } ] }); _ }, FieldAccess (Var "m", "x")) -> ()
  | _ -> Alcotest.fail "expected module signature parameter"

let neq_still_parses_shape () =
  match parse_expr "1 != 2" with
  | Ap (Ap (Var "!=", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected inequality"

let suites =
  [
    ( "parse_smoke",
      [
        Alcotest.test_case "match" `Quick (parse_ok "match (1) { x => 2 }");
        Alcotest.test_case "adt match" `Quick (parse_ok "{ type Color = Red; match (Red) { x => 1 } }");
        Alcotest.test_case "dotted field shape" `Quick dotted_field_shape;
        Alcotest.test_case "module signature sugar shape" `Quick module_signature_sugar_shape;
        Alcotest.test_case "module signature parameter shape" `Quick module_signature_param_shape;
        Alcotest.test_case "inequality still parses" `Quick neq_still_parses_shape;
        Alcotest.test_case "resume without argument rejected" `Quick (parse_fail "resume");
      ] );
  ]
