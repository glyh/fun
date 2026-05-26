open Surface

let parse_ok source () =
  ignore (Core_lexer.parse_expr source)

let effect_expr_shape () =
  match Core_lexer.parse_expr "let State S = effect get : Unit -> S; put : S -> Unit end in State I64" with
  | EffectDef { name = "State"; params = [ "S" ]; ops; body = Ap (Var "State", Explicit, Var "I64") } ->
      Alcotest.(check int) "operation count" 2 (List.length ops);
      Alcotest.(check string) "first operation" "get" (List.hd ops).name
  | _ -> Alcotest.fail "expected effect declaration"

let effect_zero_param_shape () =
  match Core_lexer.parse_expr "let Exc = effect raise : I64 -> I64 end in Exc" with
  | EffectDef { name = "Exc"; params = []; ops = [ { name = "raise"; _ } ]; body = Var "Exc" } -> ()
  | _ -> Alcotest.fail "expected zero-parameter effect declaration"

let effect_struct_shape () =
  match Core_lexer.parse_expr "struct pub let State S = effect get : Unit -> S end end" with
  | Struct { bindings = [ EffectBinding { name = "State"; params = [ "S" ]; public = true; ops = [ { name = "get"; _ } ] } ]; _ } -> ()
  | _ -> Alcotest.fail "expected public effect binding"

let () =
  Alcotest.run "syntax"
    [
      ( "parse",
        [
          Alcotest.test_case "match" `Quick (parse_ok "match 1 with x -> 2 end");
          Alcotest.test_case "adt match" `Quick (parse_ok "type Color = Red in match Red with x -> 1 end");
          Alcotest.test_case "effect expr shape" `Quick effect_expr_shape;
          Alcotest.test_case "zero-param effect shape" `Quick effect_zero_param_shape;
          Alcotest.test_case "struct effect shape" `Quick effect_struct_shape;
        ] );
    ]
