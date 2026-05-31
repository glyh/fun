let id name scope = Syntax.fresh_id ~scope name

let resolve_name tbl id =
  match Binding.resolve tbl id with
  | Some info -> Some info.resolved_name
  | None -> None

let check_resolves label expected tbl id =
  Alcotest.(check (option string)) label expected (resolve_name tbl id)

let scope_operations () =
  let a = Scope_set.of_list [ 1; 3 ] in
  let b = Scope_set.of_list [ 2; 3 ] in
  Alcotest.(check bool) "subset" true (Scope_set.subset (Scope_set.singleton 1) a);
  Alcotest.(check bool) "not subset" false (Scope_set.subset a b);
  Alcotest.(check int) "union size" 3 (Scope_set.size (Scope_set.union a b));
  Alcotest.(check bool) "contains" true (Scope_set.contains (Scope_set.union a b) 2)

let lexical_shadowing_chooses_largest_scope () =
  let tbl = Binding.create () in
  let outer = Scope_set.of_list [ 1 ] in
  let inner = Scope_set.of_list [ 1; 2 ] in
  Binding.extend tbl ~name:"x" ~scope:outer ~resolved_name:"outer";
  Binding.extend tbl ~name:"x" ~scope:inner ~resolved_name:"inner";
  check_resolves "inner occurrence" (Some "inner") tbl (id "x" inner);
  check_resolves "outer occurrence" (Some "outer") tbl (id "x" outer)

let macro_introduced_names_do_not_capture_user_names () =
  let tbl = Binding.create () in
  let user_scope = Scope_set.singleton 10 in
  let macro_scope = Scope_set.singleton 20 in
  Binding.extend tbl ~name:"tmp" ~scope:user_scope ~resolved_name:"user_tmp";
  Binding.extend tbl ~name:"tmp" ~scope:macro_scope ~resolved_name:"macro_tmp";
  check_resolves "user occurrence" (Some "user_tmp") tbl (id "tmp" user_scope);
  check_resolves "macro occurrence" (Some "macro_tmp") tbl (id "tmp" macro_scope)

let user_names_do_not_capture_macro_names () =
  let tbl = Binding.create () in
  let user_scope = Scope_set.singleton 1 in
  let macro_scope = Scope_set.singleton 2 in
  Binding.extend tbl ~name:"helper" ~scope:macro_scope ~resolved_name:"macro_helper";
  Binding.extend tbl ~name:"helper" ~scope:user_scope ~resolved_name:"user_helper";
  check_resolves "macro helper" (Some "macro_helper") tbl (id "helper" macro_scope)

let ambiguous_best_binding_is_rejected () =
  let tbl = Binding.create () in
  Binding.extend tbl ~name:"x" ~scope:(Scope_set.singleton 1) ~resolved_name:"left";
  Binding.extend tbl ~name:"x" ~scope:(Scope_set.singleton 2) ~resolved_name:"right";
  match Binding.resolve tbl (id "x" (Scope_set.of_list [ 1; 2 ])) with
  | exception Failure msg when String.starts_with ~prefix:"ambiguous binding" msg -> ()
  | exception exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
  | _ -> Alcotest.fail "expected ambiguous binding failure"

let suites =
  [
    ( "scope_set",
      [ Alcotest.test_case "operations" `Quick scope_operations ] );
    ( "binding",
      [
        Alcotest.test_case "lexical shadowing chooses largest scope" `Quick lexical_shadowing_chooses_largest_scope;
        Alcotest.test_case "macro names do not capture user names" `Quick macro_introduced_names_do_not_capture_user_names;
        Alcotest.test_case "user names do not capture macro names" `Quick user_names_do_not_capture_macro_names;
        Alcotest.test_case "ambiguous best binding rejected" `Quick ambiguous_best_binding_is_rejected;
      ] );
  ]
