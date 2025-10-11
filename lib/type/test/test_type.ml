module Testable = struct
  let type_ =
    let pp_binding ppf ty = Fmt.pf ppf "%S" (Type.T.pp ty) in
    Alcotest.testable pp_binding Type.T.equal
end

let test_type ?(tag = "same Type") t1 t2 () =
  Alcotest.check Testable.type_ tag t1 t2

let simple =
  let open Type in
  Alcotest.
    [
      test_case "Int" `Quick (test_type (T.of_human (Con "Int")) Builtin.int);
      test_case "Unit" `Quick (test_type (T.of_human (Con "Unit")) Builtin.unit);
    ]

let alpha_equivalence =
  let open Type in
  Alcotest.
    [
      test_case "forall a. a -> a ≡ forall b. b -> b" `Quick
        (test_type
           (T.of_human (Human.Forall ([ "a" ], Human.Arrow (Var "a", Var "a"))))
           (T.of_human (Human.Forall ([ "b" ], Human.Arrow (Var "b", Var "b")))));
      test_case "forall a b. a -> b ≡ forall x y. x -> y" `Quick
        (test_type
           (T.of_human
              (Human.Forall ([ "a"; "b" ], Human.Arrow (Var "a", Var "b"))))
           (T.of_human
              (Human.Forall ([ "x"; "y" ], Human.Arrow (Var "x", Var "y")))));
      test_case "forall a. a -> a ≠ forall a. a -> b (not alpha-equivalent)"
        `Quick (fun () ->
          let t1 =
            T.of_human (Human.Forall ([ "a" ], Human.Arrow (Var "a", Var "a")))
          in
          let t2 =
            T.of_human (Human.Forall ([ "a" ], Human.Arrow (Var "a", Var "b")))
          in
          Alcotest.check (Alcotest.neg Testable.type_) "different" t1 t2);
    ]

let () =
  Alcotest.run "Type"
    [ ("simple", simple); ("alpha_equivalence", alpha_equivalence) ]
