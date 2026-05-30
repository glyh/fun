open Surface

let parse_ok source () =
  ignore (Core_lexer.parse_expr source)

let parse_fail source () =
  match Core_lexer.parse_expr source with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected parse failure"

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
  match Core_lexer.parse_expr "module pub let State S = effect get : Unit -> S end end" with
  | Module { bindings = [ EffectBinding { name = "State"; params = [ "S" ]; public = true; ops = [ { name = "get"; _ } ] } ] } -> ()
  | _ -> Alcotest.fail "expected public effect binding"

let pure_arrow_shape () =
  match Core_lexer.parse_expr "I64 -> I64" with
  | Arrow (Explicit, None, Var "I64", None, Var "I64") -> ()
  | _ -> Alcotest.fail "expected pure arrow"

let single_can_shape () =
  match Core_lexer.parse_expr "I64 -> I64 can IO" with
  | Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") -> ()
  | _ -> Alcotest.fail "expected single-effect can row"

let braced_can_shape () =
  match Core_lexer.parse_expr "Unit -> I64 can {State I64, IO}" with
  | Arrow
      ( Explicit,
        None,
        Var "Unit",
        Some { effects = [ Ap (Var "State", Explicit, Var "I64"); Var "IO" ]; tail = None },
        Var "I64" ) ->
      ()
  | _ -> Alcotest.fail "expected braced effect row"

let closest_arrow_can_shape () =
  match Core_lexer.parse_expr "I64 -> I64 -> I64 can IO" with
  | Arrow
      ( Explicit,
        None,
        Var "I64",
        None,
        Arrow (Explicit, None, Var "I64", Some { effects = [ Var "IO" ]; tail = None }, Var "I64") ) ->
      ()
  | _ -> Alcotest.fail "expected can to bind to closest arrow"

let perform_get_shape () =
  match Core_lexer.parse_expr "perform State.get ()" with
  | Perform { effect_path = [ "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected perform get"

let perform_put_shape () =
  match Core_lexer.parse_expr "perform State.put 42" with
  | Perform { effect_path = [ "State" ]; op = "put"; arg = Atom (Atom.I64 42L) } -> ()
  | _ -> Alcotest.fail "expected perform put"

let perform_qualified_shape () =
  match Core_lexer.parse_expr "perform M.State.get ()" with
  | Perform { effect_path = [ "M"; "State" ]; op = "get"; arg = Atom Atom.Unit } -> ()
  | _ -> Alcotest.fail "expected qualified perform"

let dotted_field_shape () =
  match Core_lexer.parse_expr "State.get" with
  | FieldAccess (Var "State", "get") -> ()
  | _ -> Alcotest.fail "expected ordinary field access"

let effect_branch_shape () =
  match Core_lexer.parse_expr "match perform Exc.raise 1 with x -> x | effect Exc.raise n -> n end" with
  | Match
      ( Perform { effect_path = [ "Exc" ]; op = "raise"; _ },
        [ ValueBranch (PatBind "x", Var "x");
          EffectBranch { effect_path = [ "Exc" ]; op = "raise"; arg_pat = PatBind "n"; body = Var "n" } ] ) ->
      ()
  | _ -> Alcotest.fail "expected effect branch"

let qualified_effect_branch_shape () =
  match Core_lexer.parse_expr "match perform M.Exc.raise 1 with x -> x | effect M.Exc.raise n -> n end" with
  | Match (_, [ _; EffectBranch { effect_path = [ "M"; "Exc" ]; op = "raise"; _ } ]) -> ()
  | _ -> Alcotest.fail "expected qualified effect branch"

let resume_arg_shape () =
  match Core_lexer.parse_expr "match perform Exc.raise 1 with x -> x | effect Exc.raise n -> resume (n + 1) end" with
  | Match (_, [ _; EffectBranch { body = Resume (Ap (Ap (Var "+", Explicit, Var "n"), Explicit, Atom (Atom.I64 1L))); _ } ]) -> ()
  | _ -> Alcotest.fail "expected resume with argument"

let tuple_effect_branch_shape () =
  match Core_lexer.parse_expr "match perform Console.log (1, 2) with x -> x | effect Console.log (level, msg) -> level end" with
  | Match (_, [ _; EffectBranch { arg_pat = PatProd [ PatBind "level"; PatBind "msg" ]; _ } ]) -> ()
  | _ -> Alcotest.fail "expected tuple effect branch pattern"

let nominal_type_pattern_app_shape () =
  match Core_lexer.parse_expr "match T with Option x -> x | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatCon ([], "Option", [ PatBind "x" ]), Var "x"); _ ]) -> ()
  | _ -> Alcotest.fail "expected nominal type pattern application"

let nominal_type_pattern_complex_arg_shape () =
  match Core_lexer.parse_expr "match T with Option (Option I64 | I64) -> I64 | _ -> Bool end" with
  | Match (_, [ ValueBranch (PatCon ([], "Option", [ PatOr (PatCon ([], "Option", [ PatType Core.TI64 ]), PatType Core.TI64) ]), _); _ ]) -> ()
  | _ -> Alcotest.fail "expected complex nominal type pattern argument"

let struct_type_pattern_open_shape () =
  match Core_lexer.parse_expr "match T with struct x: p; _ end -> p | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("x", PatBind "p") ]; partial = true }, Var "p"); _ ]) -> ()
  | _ -> Alcotest.fail "expected open struct type pattern"

let struct_type_pattern_closed_shape () =
  match Core_lexer.parse_expr "match T with struct x: I64; y: Bool end -> I64 | _ -> Bool end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("x", PatType Core.TI64); ("y", PatType Core.TBool) ]; partial = false }, _); _ ]) -> ()
  | _ -> Alcotest.fail "expected closed struct type pattern"

let struct_type_pattern_nominal_field_shape () =
  match Core_lexer.parse_expr "match T with struct value: Option x; _ end -> x | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("value", PatCon ([], "Option", [ PatBind "x" ])) ]; partial = true }, Var "x"); _ ]) -> ()
  | _ -> Alcotest.fail "expected struct type pattern with nominal field pattern"

let trait_decl_shape () =
  match Core_lexer.parse_expr "trait Eq A = struct eq : A -> A -> Bool end in Eq" with
  | TraitDef { name = "Eq"; params = [ "A" ]; fields = [ ("eq", Arrow (Explicit, None, Var "A", None, Arrow (Explicit, None, Var "A", None, Var "Bool"))) ]; body = Var "Eq" } -> ()
  | _ -> Alcotest.fail "expected trait declaration"

let impl_decl_shape () =
  match Core_lexer.parse_expr "impl Eq I64 = struct let eq x y = x == y end in Eq.eq" with
  | ImplDef { trait_path = []; trait_name = "Eq"; args = [ Var "I64" ]; fields = [ ("eq", Lam ({ name = "x"; _ }, Lam ({ name = "y"; _ }, _))) ]; body = FieldAccess (Var "Eq", "eq") } -> ()
  | _ -> Alcotest.fail "expected impl declaration"

let single_trait_bound_shape () =
  match Core_lexer.parse_expr "{A : Eq} -> A -> A" with
  | Arrow (Implicit, Some "A", Var "Eq", None, Arrow (Explicit, None, Var "A", None, Var "A")) -> ()
  | _ -> Alcotest.fail "expected single trait bound"

let multi_trait_bound_shape () =
  match Core_lexer.parse_expr "{A : Eq + Jsonable} -> A" with
  | Arrow (Implicit, Some "A", Ap (Ap (Var "+", Explicit, Var "Eq"), Explicit, Var "Jsonable"), None, Var "A") -> ()
  | _ -> Alcotest.fail "expected multi trait bound"

let module_signature_sugar_shape () =
  match Core_lexer.parse_expr "sig x : I64; y : Bool end" with
  | Module { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = true }; LetBinding { name = "y"; value = Var "Bool"; public = true } ] } -> ()
  | _ -> Alcotest.fail "expected signature sugar as public type module"

let module_signature_param_shape () =
  match Core_lexer.parse_expr "fun (m : module let x = I64 end) -> m.x" with
  | Lam ({ name = "m"; type_ = Some (Module { bindings = [ LetBinding { name = "x"; value = Var "I64"; public = false } ] }); _ }, FieldAccess (Var "m", "x")) -> ()
  | _ -> Alcotest.fail "expected module signature parameter"

let ref_new_shape () =
  match Core_lexer.parse_expr "ref 1" with
  | RefNew (Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected ref allocation"

let ref_get_shape () =
  match Core_lexer.parse_expr "!r" with
  | RefGet (Var "r") -> ()
  | _ -> Alcotest.fail "expected deref"

let ref_set_shape () =
  match Core_lexer.parse_expr "r := 2" with
  | RefSet (Var "r", Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected assignment"

let ref_set_deref_shape () =
  match Core_lexer.parse_expr "r := !r + 1" with
  | RefSet (Var "r", Ap (Ap (Var "+", Explicit, RefGet (Var "r")), Explicit, Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected assignment with deref"

let neq_still_parses_shape () =
  match Core_lexer.parse_expr "1 != 2" with
  | Ap (Ap (Var "!=", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected inequality"

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
          Alcotest.test_case "pure arrow shape" `Quick pure_arrow_shape;
          Alcotest.test_case "single can shape" `Quick single_can_shape;
          Alcotest.test_case "braced can shape" `Quick braced_can_shape;
          Alcotest.test_case "closest arrow can shape" `Quick closest_arrow_can_shape;
          Alcotest.test_case "perform get shape" `Quick perform_get_shape;
          Alcotest.test_case "perform put shape" `Quick perform_put_shape;
          Alcotest.test_case "perform qualified shape" `Quick perform_qualified_shape;
          Alcotest.test_case "dotted field shape" `Quick dotted_field_shape;
          Alcotest.test_case "effect branch shape" `Quick effect_branch_shape;
          Alcotest.test_case "qualified effect branch shape" `Quick qualified_effect_branch_shape;
          Alcotest.test_case "resume argument shape" `Quick resume_arg_shape;
          Alcotest.test_case "tuple effect branch shape" `Quick tuple_effect_branch_shape;
          Alcotest.test_case "nominal type pattern application shape" `Quick nominal_type_pattern_app_shape;
          Alcotest.test_case "nominal type pattern complex arg shape" `Quick nominal_type_pattern_complex_arg_shape;
          Alcotest.test_case "struct type pattern open shape" `Quick struct_type_pattern_open_shape;
          Alcotest.test_case "struct type pattern closed shape" `Quick struct_type_pattern_closed_shape;
          Alcotest.test_case "struct type pattern nominal field shape" `Quick struct_type_pattern_nominal_field_shape;
          Alcotest.test_case "trait declaration shape" `Quick trait_decl_shape;
          Alcotest.test_case "impl declaration shape" `Quick impl_decl_shape;
          Alcotest.test_case "single trait bound shape" `Quick single_trait_bound_shape;
          Alcotest.test_case "multi trait bound shape" `Quick multi_trait_bound_shape;
          Alcotest.test_case "module signature sugar shape" `Quick module_signature_sugar_shape;
          Alcotest.test_case "module signature parameter shape" `Quick module_signature_param_shape;
          Alcotest.test_case "ref new shape" `Quick ref_new_shape;
          Alcotest.test_case "ref get shape" `Quick ref_get_shape;
          Alcotest.test_case "ref set shape" `Quick ref_set_shape;
          Alcotest.test_case "ref set deref shape" `Quick ref_set_deref_shape;
          Alcotest.test_case "inequality still parses" `Quick neq_still_parses_shape;
          Alcotest.test_case "resume without argument rejected" `Quick (parse_fail "resume");
        ] );
    ]
