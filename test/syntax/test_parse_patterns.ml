open Surface

let nominal_type_pattern_app_shape () =
  match Parse_expand.parse_expr "match T do Option(x) -> x | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatCon ([], "Option", [ PatBind "x" ]), Var "x"); _ ]) -> ()
  | _ -> Alcotest.fail "expected nominal type pattern application"

let nominal_type_pattern_complex_arg_shape () =
  match Parse_expand.parse_expr "match T do Option(Option(I64) | I64) -> I64 | _ -> Bool end" with
  | Match (_, [ ValueBranch (PatCon ([], "Option", [ PatOr (PatCon ([], "Option", [ PatType Atom_ty.TI64 ]), PatType Atom_ty.TI64) ]), _); _ ]) -> ()
  | _ -> Alcotest.fail "expected complex nominal type pattern argument"

let struct_type_pattern_open_shape () =
  match Parse_expand.parse_expr "match T do struct x: p; _ end -> p | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("x", PatBind "p") ]; partial = true }, Var "p"); _ ]) -> ()
  | _ -> Alcotest.fail "expected open struct type pattern"

let struct_type_pattern_closed_shape () =
  match Parse_expand.parse_expr "match T do struct x: I64; y: Bool end -> I64 | _ -> Bool end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("x", PatType Atom_ty.TI64); ("y", PatType Atom_ty.TBool) ]; partial = false }, _); _ ]) -> ()
  | _ -> Alcotest.fail "expected closed struct type pattern"

let struct_type_pattern_nominal_field_shape () =
  match Parse_expand.parse_expr "match T do struct value: Option(x); _ end -> x | _ -> I64 end" with
  | Match (_, [ ValueBranch (PatStructType { fields = [ ("value", PatCon ([], "Option", [ PatBind "x" ])) ]; partial = true }, Var "x"); _ ]) -> ()
  | _ -> Alcotest.fail "expected struct type pattern with nominal field pattern"

let suites =
  [
    ( "parse_patterns",
      [
        Alcotest.test_case "nominal type pattern application shape" `Quick nominal_type_pattern_app_shape;
        Alcotest.test_case "nominal type pattern complex arg shape" `Quick nominal_type_pattern_complex_arg_shape;
        Alcotest.test_case "struct type pattern open shape" `Quick struct_type_pattern_open_shape;
        Alcotest.test_case "struct type pattern closed shape" `Quick struct_type_pattern_closed_shape;
        Alcotest.test_case "struct type pattern nominal field shape" `Quick struct_type_pattern_nominal_field_shape;
      ] );
  ]
