open Surface

let effect_expr_shape () =
  match Parse_expand.parse_expr "let State S = effect get : Unit -> S; put : S -> Unit end in State I64" with
  | EffectDef { name = "State"; params = [ "S" ]; ops; body = Ap (Var "State", Explicit, Var "I64") } ->
      Alcotest.(check int) "operation count" 2 (List.length ops);
      Alcotest.(check string) "first operation" "get" (List.hd ops).name
  | _ -> Alcotest.fail "expected effect declaration"

let effect_zero_param_shape () =
  match Parse_expand.parse_expr "let Exc = effect raise : I64 -> I64 end in Exc" with
  | EffectDef { name = "Exc"; params = []; ops = [ { name = "raise"; _ } ]; body = Var "Exc" } -> ()
  | _ -> Alcotest.fail "expected zero-parameter effect declaration"

let effect_struct_shape () =
  match Parse_expand.parse_expr "module pub let State S = effect get : Unit -> S end end" with
  | Module { bindings = [ EffectBinding { name = "State"; params = [ "S" ]; public = true; ops = [ { name = "get"; _ } ] } ] } -> ()
  | _ -> Alcotest.fail "expected public effect binding"

let pure_arrow_shape () =
  match Parse_expand.parse_expr "I64 -> I64" with
  | Arrow (Explicit, None, Var "I64", None, Var "I64") -> ()
  | _ -> Alcotest.fail "expected pure arrow"

let single_can_shape () =
  match Parse_expand.parse_expr "I64 -> I64 can IO" with
  | Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected single-effect can row"

let braced_can_shape () =
  match Parse_expand.parse_expr "Unit -> I64 can {State I64, IO}" with
  | Arrow
      ( Explicit,
        None,
        Var "Unit",
        Some { effects = [ Ap (Var "State", Explicit, Var "I64"); Var "IO" ]; tail = None },
        Var "I64" ) ->
      ()
  | _ -> Alcotest.fail "expected braced effect row"

let pure_can_shape () =
  match Parse_expand.parse_expr "Unit -> I64 can {}" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = []; tail = None }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected pure effect row"

let open_can_shape () =
  match Parse_expand.parse_expr "Unit -> I64 can {IO | r}" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = [ Var "IO" ]; tail = Some (Var "r") }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected open effect row"

let open_can_multi_shape () =
  match Parse_expand.parse_expr "Unit -> I64 can {State I64, IO | r}" with
  | Arrow
      ( Explicit,
        None,
        Var "Unit",
        Some { effects = [ Ap (Var "State", Explicit, Var "I64"); Var "IO" ]; tail = Some (Var "r") },
        Var "I64" ) ->
      ()
  | _ -> Alcotest.fail "expected open multi-effect row"

let open_can_tail_only_shape () =
  match Parse_expand.parse_expr "Unit -> I64 can {| r}" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = []; tail = Some (Var "r") }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected tail-only effect row"

let closest_arrow_can_shape () =
  match Parse_expand.parse_expr "I64 -> I64 -> I64 can IO" with
  | Arrow
      ( Explicit,
        None,
        Var "I64",
        None,
        Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") ) ->
      ()
  | _ -> Alcotest.fail "expected can to bind to closest arrow"

let perform_get_shape () =
  match Parse_expand.parse_expr "perform State.get ()" with
  | Perform { effect_path = [ "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected perform get"

let perform_put_shape () =
  match Parse_expand.parse_expr "perform State.put 42" with
  | Perform { effect_path = [ "State" ]; op = "put"; arg = Atom (Atom.I64 42L) } -> ()
  | _ -> Alcotest.fail "expected perform put"

let perform_qualified_shape () =
  match Parse_expand.parse_expr "perform M.State.get ()" with
  | Perform { effect_path = [ "M"; "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected qualified perform"

let effect_branch_shape () =
  match Parse_expand.parse_expr "match perform Exc.raise 1 with x -> x | effect Exc.raise n -> n end" with
  | Match
      ( Perform { effect_path = [ "Exc" ]; op = "raise"; _ },
        [ ValueBranch (PatBind "x", Var "x");
          EffectBranch { effect_path = [ "Exc" ]; op = "raise"; arg_pat = PatBind "n"; body = Var "n" } ] ) ->
      ()
  | _ -> Alcotest.fail "expected effect branch"

let qualified_effect_branch_shape () =
  match Parse_expand.parse_expr "match perform M.Exc.raise 1 with x -> x | effect M.Exc.raise n -> n end" with
  | Match (_, [ _; EffectBranch { effect_path = [ "M"; "Exc" ]; op = "raise"; _ } ]) -> ()
  | _ -> Alcotest.fail "expected qualified effect branch"

let resume_arg_shape () =
  match Parse_expand.parse_expr "match perform Exc.raise 1 with x -> x | effect Exc.raise n -> resume (n + 1) end" with
  | Match (_, [ _; EffectBranch { body = Resume (Ap (Ap (Var "+", Explicit, Var "n"), Explicit, Atom (Atom.I64 1L))); _ } ]) -> ()
  | _ -> Alcotest.fail "expected resume with argument"

let tuple_effect_branch_shape () =
  match Parse_expand.parse_expr "match perform Console.log (1, 2) with x -> x | effect Console.log (level, msg) -> level end" with
  | Match (_, [ _; EffectBranch { arg_pat = PatProd [ PatBind "level"; PatBind "msg" ]; _ } ]) -> ()
  | _ -> Alcotest.fail "expected tuple effect branch pattern"

let suites =
  [
    ( "parse_effects",
      [
        Alcotest.test_case "effect expr shape" `Quick effect_expr_shape;
        Alcotest.test_case "zero-param effect shape" `Quick effect_zero_param_shape;
        Alcotest.test_case "struct effect shape" `Quick effect_struct_shape;
        Alcotest.test_case "pure arrow shape" `Quick pure_arrow_shape;
        Alcotest.test_case "single can shape" `Quick single_can_shape;
        Alcotest.test_case "braced can shape" `Quick braced_can_shape;
        Alcotest.test_case "pure can shape" `Quick pure_can_shape;
        Alcotest.test_case "open can shape" `Quick open_can_shape;
        Alcotest.test_case "open can multi shape" `Quick open_can_multi_shape;
        Alcotest.test_case "open can tail-only shape" `Quick open_can_tail_only_shape;
        Alcotest.test_case "closest arrow can shape" `Quick closest_arrow_can_shape;
        Alcotest.test_case "perform get shape" `Quick perform_get_shape;
        Alcotest.test_case "perform put shape" `Quick perform_put_shape;
        Alcotest.test_case "perform qualified shape" `Quick perform_qualified_shape;
        Alcotest.test_case "effect branch shape" `Quick effect_branch_shape;
        Alcotest.test_case "qualified effect branch shape" `Quick qualified_effect_branch_shape;
        Alcotest.test_case "resume argument shape" `Quick resume_arg_shape;
        Alcotest.test_case "tuple effect branch shape" `Quick tuple_effect_branch_shape;
      ] );
  ]
