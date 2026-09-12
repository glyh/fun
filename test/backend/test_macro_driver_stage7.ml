open Core
open Atom

let stx kind = { Syntax.kind; span = Source_span.synthetic }
let id name = Syntax.fresh_id name

let has_prefix prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let expect_fuel_failure f =
  match f () with
  | exception Failure msg
      when has_prefix "macro expansion exceeded fuel limit" msg -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected macro expansion fuel failure"

let expr_call name =
  stx (Syntax.MacroCall (stx (Syntax.Var (id name)), [ stx (Syntax.Atom (I64 0L)) ]))

let ready_expr_macro_ctx () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ _ ->
    VStx (StxExpr (stx (Syntax.Atom (I64 1L)))));
  Expand_ctx.register_macro ctx ~name:"mk" ~value:(VAtom Unit);
  Expand_ctx.register_macro_kind ctx ~name:"mk" ~kind:Syntax.MacroKind.default;
  ctx

let test_expression_macro_fuel_exhausts () =
  let ctx = ready_expr_macro_ctx () in
  ctx.Expand_ctx.macro_fuel := 0;
  expect_fuel_failure (fun () -> ignore (Expand.expand ctx (expr_call "mk")))

let test_expression_macro_fuel_allows_one () =
  let ctx = ready_expr_macro_ctx () in
  ctx.Expand_ctx.macro_fuel := 1;
  match (Expand.expand ctx (expr_call "mk")).Syntax.kind with
  | Syntax.Atom (I64 1L) ->
      Alcotest.(check int) "fuel restored after expansion" 1 !(ctx.Expand_ctx.macro_fuel)
  | _ -> Alcotest.fail "expected macro expansion to produce i64 syntax"

let test_macro_fuel_shared_across_copy () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.macro_fuel := 1;
  let copied = Expand_ctx.copy ctx in
  Expand_ctx.with_macro_fuel copied ~name:"mk" (fun () ->
    Alcotest.(check int) "parent sees copied-context reservation" 0 !(ctx.Expand_ctx.macro_fuel));
  Alcotest.(check int) "parent sees release" 1 !(ctx.Expand_ctx.macro_fuel)

let test_decl_macro_fuel_exhausts () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ _ -> VStx (StxDecls []));
  Expand_ctx.set_expansion_position ctx Syntax.MacroKind.Decl;
  Expand_ctx.register_macro ctx ~name:"gen" ~value:(VAtom Unit);
  Expand_ctx.register_macro_kind ctx ~name:"gen" ~kind:Syntax.MacroKind.Decl;
  ctx.Expand_ctx.macro_fuel := 0;
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = [] } in
  expect_fuel_failure (fun () -> ignore (Expand.expand_struct_bindings ctx [ call ]))

let test_driver_provisional_filled_and_cleared () =
  let output = Macro_driver.run (Enforest.parse_module "macro mk(_) -> Syntax.i64(1)\n") in
  Alcotest.(check bool) "macro exported" true
    (List.exists (fun (e : Macro_driver.macro_export) -> String.equal e.name "mk") output.macro_exports);
  Alcotest.(check bool) "no pending marker remains" false
    (Expand_ctx.is_provisional_macro output.expand_ctx "mk")

let test_provisional_rollback_restores_previous_macro () =
  let ctx = Expand_ctx.create () in
  let calls = ref 0 in
  ctx.Expand_ctx.elaborate <- Some (fun _ ->
    incr calls;
    if !calls = 1 then VAtom Unit else failwith "compile boom");
  let macro_binding value =
    Syntax.MacroBinding { name = id "mk"; value; public = false; kind = None }
  in
  ignore (Expand.expand_struct_bindings ctx [ macro_binding (stx (Syntax.Atom (I64 1L))) ]);
  let previous_kind = Expand_ctx.lookup_macro_kind ctx "mk" in
  (match Expand.expand_struct_bindings ctx [ macro_binding (stx (Syntax.Atom (I64 2L))) ] with
   | exception Failure msg when String.equal msg "compile boom" -> ()
   | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
   | _ -> Alcotest.fail "expected compile failure");
  Alcotest.(check bool) "ready macro restored" true
    (Option.is_some (Expand_ctx.lookup_macro_entry ctx "mk"));
  Alcotest.(check bool) "kind restored" true
    (Expand_ctx.lookup_macro_kind ctx "mk" = previous_kind);
  Alcotest.(check bool) "pending marker cleared" false
    (Expand_ctx.is_provisional_macro ctx "mk")

let test_provisional_rollback_removes_fresh_failure () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.elaborate <- Some (fun _ -> failwith "compile boom");
  let macro_binding =
    Syntax.MacroBinding {
      name = id "fresh";
      value = stx (Syntax.Atom (I64 1L));
      public = false;
      kind = None;
    }
  in
  (match Expand.expand_struct_bindings ctx [ macro_binding ] with
   | exception Failure msg when String.equal msg "compile boom" -> ()
   | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
   | _ -> Alcotest.fail "expected compile failure");
  Alcotest.(check bool) "no ready macro remains" false
    (Option.is_some (Expand_ctx.lookup_macro_entry ctx "fresh"));
  Alcotest.(check bool) "no kind remains" false
    (Option.is_some (Expand_ctx.lookup_macro_kind ctx "fresh"));
  Alcotest.(check bool) "no pending marker remains" false
    (Expand_ctx.is_provisional_macro ctx "fresh")

let () =
  Alcotest.run "macro_driver_stage7"
    [ ( "fuel_and_provisional",
        [ Alcotest.test_case "expr macro fuel exhausts" `Quick test_expression_macro_fuel_exhausts;
          Alcotest.test_case "expr macro fuel allows one" `Quick test_expression_macro_fuel_allows_one;
          Alcotest.test_case "fuel shared across copy" `Quick test_macro_fuel_shared_across_copy;
          Alcotest.test_case "decl macro fuel exhausts" `Quick test_decl_macro_fuel_exhausts;
          Alcotest.test_case "provisional filled and cleared" `Quick test_driver_provisional_filled_and_cleared;
          Alcotest.test_case "provisional rollback restores previous" `Quick test_provisional_rollback_restores_previous_macro;
          Alcotest.test_case "provisional rollback removes fresh failure" `Quick test_provisional_rollback_removes_fresh_failure;
        ] ) ]
