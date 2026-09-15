open Core
open Atom

let stx kind = { Syntax.kind; span = Source_span.synthetic }
let id name = Syntax.fresh_id name

(* Macro applications are calls under the evaluation budget (M5). The tests run
   them under a tiny limit so exhaustion is cheap to reach. *)
let with_limit ctx limit f = Eval_budget.start ~limit:(Some limit) ~demand:"a test" ctx.Expand_ctx.budget f

(* An overrun inside an application is that application's error (ticket
   expansion-errors-reach-the-user-raw): it names the macro, not a raw budget
   exception. *)
let expect_budget_exceeded ~call f =
  match f () with
  | exception Expand_error.Error { error = BudgetExceeded { call = c; _ }; _ } ->
      Alcotest.(check string) "names the call" call c
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an evaluation budget error"

let expr_call name =
  stx (Syntax.MacroCall (stx (Syntax.Var (id name)), [ Syntax.CapExpr (stx (Syntax.Atom (I64 0L))) ]))

let ready_expr_macro_ctx () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ _ _ ->
    VStx (StxExpr (stx (Syntax.Atom (I64 1L)))));
  Expand_ctx.register_macro ctx ~name:"mk" ~value:(VAtom Unit);
  Expand_ctx.register_macro_kind ctx ~name:"mk" ~kind:Syntax.MacroKind.default ~params:[ Syntax.HoleExpr ];
  ctx

let test_expression_macro_exhausts_budget () =
  let ctx = ready_expr_macro_ctx () in
  expect_budget_exceeded ~call:"macro 'mk'" (fun () ->
      with_limit ctx 0 (fun () -> Expand.expand ctx (expr_call "mk")))

let test_expression_macro_budget_allows_one () =
  let ctx = ready_expr_macro_ctx () in
  match (with_limit ctx 1 (fun () -> Expand.expand ctx (expr_call "mk"))).Syntax.kind with
  | Syntax.Atom (I64 1L) -> ()
  | _ -> Alcotest.fail "expected macro expansion to produce i64 syntax"

(* A copied context spends from the same budget: two applications, one through
   the copy, exceed a limit of one. *)
let test_budget_shared_across_copy () =
  let ctx = ready_expr_macro_ctx () in
  let copied = Expand_ctx.copy ctx in
  expect_budget_exceeded ~call:"macro 'mk'" (fun () ->
      with_limit ctx 1 (fun () ->
          ignore (Expand.expand copied (expr_call "mk"));
          Expand.expand ctx (expr_call "mk")))

let test_decl_macro_exhausts_budget () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ _ _ -> VStx (StxDecls []));
  Expand_ctx.register_macro ctx ~name:"gen" ~value:(VAtom Unit);
  Expand_ctx.register_macro_kind ctx ~name:"gen" ~kind:Syntax.MacroKind.Decl ~params:[];
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = [] } in
  expect_budget_exceeded ~call:"macro 'gen'" (fun () ->
      with_limit ctx 0 (fun () -> Expand.expand_struct_bindings ctx [ call ]))

(* Breadth blowup at bounded depth: [mk(n)] expands to [mk(n-1) + mk(n-1)], so
   depth [n] is 2^(n+1) - 1 applications nested only [n] deep. A nesting guard
   never trips on it; the budget counts every application. *)
let doubling_macro_ctx () =
  let ctx = Expand_ctx.create () in
  let call n = stx (Syntax.MacroCall (stx (Syntax.Var (id "mk")), [ Syntax.CapExpr (stx (Syntax.Atom (I64 n))) ])) in
  let plus a b = stx (Syntax.Ap (stx (Syntax.Ap (stx (Syntax.Var (id "+")), Explicit, a)), Explicit, b)) in
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ _ arg ->
    match arg with
    | VStx (StxExpr { kind = Syntax.Atom (I64 0L); _ }) -> VStx (StxExpr (stx (Syntax.Atom (I64 1L))))
    | VStx (StxExpr { kind = Syntax.Atom (I64 n); _ }) ->
        VStx (StxExpr (plus (call (Int64.pred n)) (call (Int64.pred n))))
    | _ -> VAtom Unit);
  (* Bound with an empty scope set, so the macro's own output finds it. *)
  Binding.extend ctx.Expand_ctx.binding_table ~name:"mk" ~scope:Scope_set.empty ~kind:Binding.Macro
    ~resolved_name:"mk";
  Expand_ctx.register_macro ctx ~name:"mk" ~value:(VAtom Unit);
  Expand_ctx.register_macro_kind ctx ~name:"mk" ~kind:Syntax.MacroKind.default ~params:[ Syntax.HoleExpr ];
  (ctx, call)

let test_breadth_within_budget () =
  let ctx, call = doubling_macro_ctx () in
  ignore (with_limit ctx 15 (fun () -> Expand.expand ctx (call 3L)))

let test_breadth_blowup_exceeds_budget () =
  let ctx, call = doubling_macro_ctx () in
  expect_budget_exceeded ~call:"macro 'mk'" (fun () ->
      with_limit ctx 100 (fun () -> Expand.expand ctx (call 10L)))

(* The body runs with fresh metas but spends from the expansion's budget: an
   identity macro is one application plus one call, so a limit of one is spent
   by the application and its body exceeds it. *)
let test_macro_body_spends_from_the_expansion () =
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.eval_and_apply <- Some Nbe.apply_macro;
  Expand_ctx.register_macro ctx ~name:"mk" ~value:(VLam { body = { env = []; body = Var 0 } });
  Expand_ctx.register_macro_kind ctx ~name:"mk" ~kind:Syntax.MacroKind.default ~params:[ Syntax.HoleExpr ];
  ignore (with_limit ctx 2 (fun () -> Expand.expand ctx (expr_call "mk")));
  match with_limit ctx 1 (fun () -> Expand.expand ctx (expr_call "mk")) with
  | exception Expand_error.Error { error = BudgetExceeded { macro; call; _ }; _ } ->
      Alcotest.(check string) "names the application" "mk" macro;
      Alcotest.(check bool) "names the body, not the application" false (String.equal call "macro 'mk'")
  | _ -> Alcotest.fail "expected an evaluation budget error"

let test_driver_provisional_filled_and_cleared () =
  let output = Macro_driver.run ~load_syntax:Elab_prelude.std_load_syntax (Enforest.parse_module "open (import \"std\");\nmacro mk(_) { Syntax.i64(1) }\n") in
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
    Syntax.MacroBinding { name = id "mk"; value; public = false; kind = None; output = None }
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
      output = None;
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
    [ ( "budget_and_provisional",
        [ Alcotest.test_case "expr macro exhausts the budget" `Quick test_expression_macro_exhausts_budget;
          Alcotest.test_case "expr macro budget allows one" `Quick test_expression_macro_budget_allows_one;
          Alcotest.test_case "budget shared across copy" `Quick test_budget_shared_across_copy;
          Alcotest.test_case "decl macro exhausts the budget" `Quick test_decl_macro_exhausts_budget;
          Alcotest.test_case "breadth within the budget" `Quick test_breadth_within_budget;
          Alcotest.test_case "breadth blowup exceeds the budget" `Quick test_breadth_blowup_exceeds_budget;
          Alcotest.test_case "macro body spends from the expansion" `Quick test_macro_body_spends_from_the_expansion;
          Alcotest.test_case "provisional filled and cleared" `Quick test_driver_provisional_filled_and_cleared;
          Alcotest.test_case "provisional rollback restores previous" `Quick test_provisional_rollback_restores_previous_macro;
          Alcotest.test_case "provisional rollback removes fresh failure" `Quick test_provisional_rollback_removes_fresh_failure;
        ] ) ]
