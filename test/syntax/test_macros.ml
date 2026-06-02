let parse source =
  let elaborate expr =
    let ctx = Elaborate.init_ctx () in
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  Parse_expand.parse_expr ~elaborate ~eval_and_apply source

let identity_macro_shape () =
  match parse "do macro id(stx) -> stx; id @ (42) end" with
  | Surface.Atom (Atom.I64 42L) -> ()
  | _ -> Alcotest.fail "expected atom 42 from identity macro"

let macro_call_with_compound_arg_shape () =
  match parse "do macro id(stx) -> stx; id @ (1 + 2) end" with
  | Surface.Ap (Surface.Ap (Surface.Var "+", _, Surface.Atom (Atom.I64 1L)), _, Surface.Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected 1 + 2 from macro call"

let stx_make_i64_literal () =
  match parse "do macro m(_) -> stx_make_i64(42); m @ (0) end" with
  | Surface.Atom (Atom.I64 42L) -> ()
  | _ -> Alcotest.fail "expected 42"

let stx_make_ap_plus () =
  match parse "do macro ap(_) -> stx_make_ap(stx_make_ap(stx_make_var(\"+\"), stx_make_i64(1)), stx_make_i64(2)); ap @ (0) end" with
  | Surface.Ap (Surface.Ap (Surface.Var "+", _, Surface.Atom (Atom.I64 1L)), _, Surface.Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected 1 + 2"

let stx_make_lam_identity () =
  match parse "do macro mk(_) -> stx_make_lam(\"x\", stx_make_var(\"x\")); mk @ (0) end" with
  | Surface.Lam ({ name = "x"; _ }, Surface.Var "x") -> ()
  | _ -> Alcotest.fail "expected fun x -> x"

let suites =
  [ ( "parse_macros",
      [ Alcotest.test_case "identity macro" `Quick identity_macro_shape;
        Alcotest.test_case "macro call with compound arg" `Quick macro_call_with_compound_arg_shape;
      ] );
    ( "parse_macro_prims",
      [ Alcotest.test_case "stx_make_i64 literal" `Quick stx_make_i64_literal;
        Alcotest.test_case "stx_make_ap +" `Quick stx_make_ap_plus;
        Alcotest.test_case "stx_make_lam identity" `Quick stx_make_lam_identity;
      ] );
  ]
