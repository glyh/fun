open Surface

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

let suites =
  [
    ( "parse_traits",
      [
        Alcotest.test_case "trait declaration shape" `Quick trait_decl_shape;
        Alcotest.test_case "impl declaration shape" `Quick impl_decl_shape;
        Alcotest.test_case "single trait bound shape" `Quick single_trait_bound_shape;
        Alcotest.test_case "multi trait bound shape" `Quick multi_trait_bound_shape;
      ] );
    ( "parse_refs",
      [
        Alcotest.test_case "ref new shape" `Quick ref_new_shape;
        Alcotest.test_case "ref get shape" `Quick ref_get_shape;
        Alcotest.test_case "ref set shape" `Quick ref_set_shape;
        Alcotest.test_case "ref set deref shape" `Quick ref_set_deref_shape;
      ] );
  ]
