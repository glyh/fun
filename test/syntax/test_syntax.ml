let suites =
  Test_parse_smoke.suites
  @ Test_parse_effects.suites
  @ Test_parse_patterns.suites
  @ Test_parse_traits_refs.suites
  @ Test_scope_sets.suites
  @ Test_expand_compat.suites
  @ Test_macros.suites

let () = Alcotest.run "syntax" suites
