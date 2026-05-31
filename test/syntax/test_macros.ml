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
  match parse "macro id = fun stx -> stx in id @ (42)" with
  | Surface.Atom (Atom.I64 42L) -> ()
  | _ -> Alcotest.fail "expected atom 42 from identity macro"

let macro_call_with_compound_arg_shape () =
  match parse "macro id = fun stx -> stx in id @ (1 + 2)" with
  | Surface.Ap (Surface.Ap (Surface.Var "+", _, Surface.Atom (Atom.I64 1L)), _, Surface.Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected 1 + 2 from macro call"

let suites =
  [ ( "parse_macros",
      [ Alcotest.test_case "identity macro" `Quick identity_macro_shape;
        Alcotest.test_case "macro call with compound arg" `Quick macro_call_with_compound_arg_shape;
      ] );
  ]
