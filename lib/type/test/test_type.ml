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
      test_case "I64" `Quick
        (test_type (T.of_human (Con ("I64", []))) Generic.i64);
      test_case "Unit" `Quick
        (test_type (T.of_human (Con ("Unit", []))) Generic.unit);
    ]

let malformed =
  let open Type in
  Alcotest.
    [
      test_case "nested forall rejected" `Quick (fun () ->
          let rank2 =
            T.of_human
              Generic.(
                Forall
                  ( [ "a" ],
                    Arrow (Var "a", Forall ([ "b" ], Arrow (Var "b", Var "b")))
                  ))
          in
          Alcotest.check_raises "reject rank-2 during equality"
            Type.Exceptions.Rank2TypeUnsupported (fun () ->
              ignore (T.equal rank2 rank2)));
    ]

let alpha_equivalence =
  let open Type in
  Alcotest.
    [
      test_case "forall a. a -> a ≡ forall b. b -> b" `Quick
        (test_type
           (T.of_human Generic.(Forall ([ "a" ], Arrow (Var "a", Var "a"))))
           (T.of_human Generic.(Forall ([ "b" ], Arrow (Var "b", Var "b")))));
      test_case "forall a b. a -> b ≡ forall x y. x -> y" `Quick
        (test_type
           (T.of_human
              Generic.(Forall ([ "a"; "b" ], Arrow (Var "a", Var "b"))))
           (T.of_human
              Generic.(Forall ([ "x"; "y" ], Arrow (Var "x", Var "y")))));
      test_case "forall a. a -> a ≠ forall a. a -> b (not alpha-equivalent)"
        `Quick (fun () ->
          let t1 =
            T.of_human Generic.(Forall ([ "a" ], Arrow (Var "a", Var "a")))
          in
          let t2 =
            T.of_human Generic.(Forall ([ "a" ], Arrow (Var "a", Var "b")))
          in
          Alcotest.check (Alcotest.neg Testable.type_) "different" t1 t2);
    ]

let () =
  Alcotest.run "Type"
    [
      ("simple", simple);
      ("alpha_equivalence", alpha_equivalence);
      ("malformed", malformed);
    ]
