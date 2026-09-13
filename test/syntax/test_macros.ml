let unwrap_std (e : Shape.t) : Shape.t =
  match e with Shape.Open (Shape.Import "std", body, _) -> body | other -> other
let parse source =
  let ctx = Elaborate.init_ctx () in
  let syntax_nominals = Elaborate.syntax_nominals ctx in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  unwrap_std (Parse_written.parse_expr ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax ~elaborate ~eval_and_apply ~syntax_nominals source)

let identity_macro_shape () =
  match parse "do macro id(stx) -> stx; id(42) end" with
  | Shape.Atom (Atom.I64 42L) -> ()
  | _ -> Alcotest.fail "expected atom 42 from identity macro"

let macro_call_with_compound_arg_shape () =
  match parse "do macro id(stx) -> stx; id(1 + 2) end" with
  | Shape.Ap (Shape.Ap (Shape.Var "+", _, Shape.Atom (Atom.I64 1L)), _, Shape.Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected 1 + 2 from macro call"

let syntax_i64_literal () =
  match parse "do macro m(_) -> Syntax.i64(42); m(0) end" with
  | Shape.Atom (Atom.I64 42L) -> ()
  | _ -> Alcotest.fail "expected 42"

let syntax_ap_plus () =
  match parse "do macro ap(_) -> Syntax.ap(Syntax.ap(Syntax.var(\"+\"), Syntax.i64(1)), Syntax.i64(2)); ap(0) end" with
  | Shape.Ap (Shape.Ap (Shape.Var "+", _, Shape.Atom (Atom.I64 1L)), _, Shape.Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected 1 + 2"

let syntax_lam_identity () =
  match parse "do macro mk(_) -> Syntax.lam(\"x\", Syntax.var(\"x\")); mk(0) end" with
  | Shape.Lam ({ name = "x"; _ }, Shape.Var "x") -> ()
  | _ -> Alcotest.fail "expected fun x -> x"

let suites =
  [ ( "parse_macros",
      [ Alcotest.test_case "identity macro" `Quick identity_macro_shape;
        Alcotest.test_case "macro call with compound arg" `Quick macro_call_with_compound_arg_shape;
      ] );
    ( "parse_macro_syntax_api",
      [ Alcotest.test_case "Syntax.i64 literal" `Quick syntax_i64_literal;
        Alcotest.test_case "Syntax.ap +" `Quick syntax_ap_plus;
        Alcotest.test_case "Syntax.lam identity" `Quick syntax_lam_identity;
      ] );
  ]
