let unwrap_std (e : Shape.t) : Shape.t = match e with Shape.Open (Shape.Import "std", body, _) -> body | other -> other
let parse_expr source = unwrap_std (Parse_written.parse_expr ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source)
let parse_module source = Parse_written.parse_module ~load_syntax:Elab_prelude.std_load_syntax source
open Shape

let effect_expr_shape () =
  match parse_expr "{ effect State(S) = sig { get : Unit -> S; put : S -> Unit }; State(I64) }" with
  | EffectDef { name = "State"; params = [ "S" ]; ops; body = Ap (Var "State", Explicit, Var "I64") } ->
      Alcotest.(check int) "operation count" 2 (List.length ops);
      Alcotest.(check string) "first operation" "get" (List.hd ops).name
  | _ -> Alcotest.fail "expected effect declaration"

let effect_zero_param_shape () =
  match parse_expr "{ effect Exc = sig { raise : I64 -> I64 }; Exc }" with
  | EffectDef { name = "Exc"; params = []; ops = [ { name = "raise"; _ } ]; body = Var "Exc" } -> ()
  | _ -> Alcotest.fail "expected zero-parameter effect declaration"

let effect_struct_shape () =
  match parse_expr "module { pub effect State(S) = sig { get : Unit -> S } }" with
  | Module { bindings = [ EffectBinding { name = "State"; params = [ "S" ]; public = true; ops = [ { name = "get"; _ } ] } ] } -> ()
  | _ -> Alcotest.fail "expected public effect binding"

let pure_arrow_shape () =
  match parse_expr "I64 -> I64" with
  | Arrow (Explicit, None, Var "I64", None, Var "I64") -> ()
  | _ -> Alcotest.fail "expected pure arrow"

let single_row_shape () =
  match parse_expr "I64 ->{IO} I64" with
  | Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected single-effect row"

let braced_row_shape () =
  match parse_expr "Unit ->{State(I64), IO} I64" with
  | Arrow
      ( Explicit,
        None,
        Var "Unit",
        Some { effects = [ Ap (Var "State", Explicit, Var "I64"); Var "IO" ]; tail = None },
        Var "I64" ) ->
      ()
  | _ -> Alcotest.fail "expected braced effect row"

let pure_row_shape () =
  match parse_expr "Unit ->{} I64" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = []; tail = None }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected pure effect row"

let open_row_shape () =
  match parse_expr "Unit ->{IO | r} I64" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = [ Var "IO" ]; tail = Some (Var "r") }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected open effect row"

let open_row_multi_shape () =
  match parse_expr "Unit ->{State(I64), IO | r} I64" with
  | Arrow
      ( Explicit,
        None,
        Var "Unit",
        Some { effects = [ Ap (Var "State", Explicit, Var "I64"); Var "IO" ]; tail = Some (Var "r") },
        Var "I64" ) ->
      ()
  | _ -> Alcotest.fail "expected open multi-effect row"

let open_row_tail_only_shape () =
  match parse_expr "Unit ->{| r} I64" with
  | Arrow (Explicit, None, Var "Unit", Some { effects = []; tail = Some (Var "r") }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected tail-only effect row"

let closest_arrow_row_shape () =
  match parse_expr "I64 -> I64 ->{IO} I64" with
  | Arrow
      ( Explicit,
        None,
        Var "I64",
        None,
        Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") ) ->
      ()
  | _ -> Alcotest.fail "expected the row on its arrow"

let perform_get_shape () =
  match parse_expr "perform State.get()" with
  | Perform { effect_path = [ "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected perform get"

let perform_put_shape () =
  match parse_expr "perform State.put(42)" with
  | Perform { effect_path = [ "State" ]; op = "put"; arg = Atom (Atom.I64 42L) } -> ()
  | _ -> Alcotest.fail "expected perform put"

let perform_qualified_shape () =
  match parse_expr "perform M.State.get()" with
  | Perform { effect_path = [ "M"; "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected qualified perform"

let effect_branch_shape () =
  match parse_expr "match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n }" with
  | Match
      ( Perform { effect_path = [ "Exc" ]; op = "raise"; _ },
        [ ValueBranch (PatBind "x", Var "x");
          EffectBranch { effect_path = [ "Exc" ]; op = "raise"; arg_pat = PatBind "n"; body = Var "n" } ] ) ->
      ()
  | _ -> Alcotest.fail "expected effect branch"

let qualified_effect_branch_shape () =
  match parse_expr "match (perform M.Exc.raise(1)) { x => x, effect M.Exc.raise n => n }" with
  | Match (_, [ _; EffectBranch { effect_path = [ "M"; "Exc" ]; op = "raise"; _ } ]) -> ()
  | _ -> Alcotest.fail "expected qualified effect branch"

let resume_arg_shape () =
  match parse_expr "match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(n + 1) }" with
  | Match (_, [ _; EffectBranch { body = Resume (Ap (Ap (Var "+", Explicit, Var "n"), Explicit, Atom (Atom.I64 1L))); _ } ]) -> ()
  | _ -> Alcotest.fail "expected resume with argument"

let tuple_effect_branch_shape () =
  match parse_expr "match (perform Console.log((1, 2))) { x => x, effect Console.log (level, msg) => level }" with
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
        Alcotest.test_case "single row shape" `Quick single_row_shape;
        Alcotest.test_case "braced row shape" `Quick braced_row_shape;
        Alcotest.test_case "pure row shape" `Quick pure_row_shape;
        Alcotest.test_case "open row shape" `Quick open_row_shape;
        Alcotest.test_case "open row multi shape" `Quick open_row_multi_shape;
        Alcotest.test_case "open row tail-only shape" `Quick open_row_tail_only_shape;
        Alcotest.test_case "closest arrow row shape" `Quick closest_arrow_row_shape;
        Alcotest.test_case "perform get shape" `Quick perform_get_shape;
        Alcotest.test_case "perform put shape" `Quick perform_put_shape;
        Alcotest.test_case "perform qualified shape" `Quick perform_qualified_shape;
        Alcotest.test_case "effect branch shape" `Quick effect_branch_shape;
        Alcotest.test_case "qualified effect branch shape" `Quick qualified_effect_branch_shape;
        Alcotest.test_case "resume argument shape" `Quick resume_arg_shape;
        Alcotest.test_case "tuple effect branch shape" `Quick tuple_effect_branch_shape;
      ] );
  ]
