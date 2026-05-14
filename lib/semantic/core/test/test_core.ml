open Core_tt.Core
open Core_tt.Nbe
open Core_tt.Unify

let mc () = MetaContext.create ()

let parse_expr source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer

let eval_source source =
  let expr = parse_expr source in
  let ctx = Core_tt.Elaborate.init_ctx () in
  let core, _ty = Core_tt.Elaborate.on_expr ctx expr in
  Core_tt.Elaborate.Ctx.eval ctx core

let check_i64 label expected source () =
  match eval_source source with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | _ -> Alcotest.fail (Printf.sprintf "%s: expected VAtom I64" label)

let check_conv label s1 s2 () =
  let ctx = Core_tt.Elaborate.init_ctx () in
  let core1, _ = Core_tt.Elaborate.on_expr ctx (parse_expr s1) in
  let core2, _ = Core_tt.Elaborate.on_expr ctx (parse_expr s2) in
  let v1 = Core_tt.Elaborate.Ctx.eval ctx core1 in
  let v2 = Core_tt.Elaborate.Ctx.eval ctx core2 in
  Alcotest.(check bool) label true (Core_tt.Elaborate.Ctx.conv ctx v1 v2)

let check_not_conv label s1 s2 () =
  let ctx = Core_tt.Elaborate.init_ctx () in
  let core1, _ = Core_tt.Elaborate.on_expr ctx (parse_expr s1) in
  let core2, _ = Core_tt.Elaborate.on_expr ctx (parse_expr s2) in
  let v1 = Core_tt.Elaborate.Ctx.eval ctx core1 in
  let v2 = Core_tt.Elaborate.Ctx.eval ctx core2 in
  Alcotest.(check bool) label false (Core_tt.Elaborate.Ctx.conv ctx v1 v2)

(* -- eval tests --------------------------------------------------------- *)

let test_eval_prod () =
  match eval_source "(1, true)" with
  | VProd [ VAtom (I64 1L); VAtom (Bool true) ] -> ()
  | _ -> Alcotest.fail "expected VProd"

let test_eval_pi () =
  match eval_source "I64 -> Bool" with
  | VPi { domain = VAtomTy TI64; _ } -> ()
  | _ -> Alcotest.fail "expected VPi"

let test_eval_dot () =
  match eval_source "let M = struct pub let x = 99 end in M.x" with
  | VAtom (I64 n) -> Alcotest.(check int64) "dot" 99L n
  | _ -> Alcotest.fail "expected VAtom"

(* -- neutral tests (require manual construction) ------------------------- *)

let test_neutral_var () =
  let mc = mc () in
  let v = eval mc [ VRigid { lvl = 0; spine = [] } ] (Var 0) in
  match v with VRigid { lvl = 0; spine = [] } -> () | _ -> Alcotest.fail "expected VRigid"

let test_neutral_ap () =
  let mc = mc () in
  let env = [ VRigid { lvl = 0; spine = [] } ] in
  let v = eval mc env (Ap (Var 0, Explicit, Atom (I64 1L))) in
  match v with
  | VNeutral { neutral = { head = HVar 0; frames = [ FApp (VAtom (I64 1L)) ] }; _ } -> ()
  | VRigid { lvl = 0; spine = [ VAtom (I64 1L) ] } -> ()
  | _ -> Alcotest.fail "expected stuck application"

let test_neutral_if () =
  let mc = mc () in
  let env = [ VRigid { lvl = 0; spine = [] } ] in
  let v = eval mc env (If (Var 0, Atom (I64 1L), Atom (I64 2L))) in
  match v with
  | VNeutral { neutral = { head = HVar 0; frames = [ FIf _ ] }; _ } -> ()
  | _ -> Alcotest.fail "expected stuck if"

(* -- meta tests (require manual construction) ---------------------------- *)

let test_meta_solve () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v = eval mc [] (Meta id) in
  (match v with VFlex { id = 0; spine = [] } -> () | _ -> Alcotest.fail "expected VFlex");
  MetaContext.solve mc id (VAtomTy TI64);
  let v2 = force mc v in
  match v2 with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected VAtomTy after solve"

let test_meta_conv () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  MetaContext.solve mc id (VAtomTy TI64);
  let v1 = eval mc [] (Meta id) in
  let v2 = VAtomTy TI64 in
  Alcotest.(check bool) "meta conv" true (conv mc 0 v1 v2)

let test_inserted_meta () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let env = [ VAtom (I64 42L); VRigid { lvl = 0; spine = [] } ] in
  let bds = [ Defined; Bound ] in
  let v = eval mc env (InsertedMeta (id, bds)) in
  (match v with VFlex { id = 0; spine = [ VRigid { lvl = 0; spine = [] } ] } -> () | _ -> Alcotest.fail "expected VFlex with bound var");
  MetaContext.solve mc id (VLam { body = { env = []; body = Var 0 } });
  let v2 = eval mc env (InsertedMeta (id, bds)) in
  let v2 = force mc v2 in
  match v2 with VRigid { lvl = 0; spine = [] } -> () | _ -> Alcotest.fail "expected VRigid after solve"

(* -- unify tests (require manual construction) --------------------------- *)

let test_unify_simple () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [] } in
  let v2 = VAtomTy TI64 in
  unify mc [] 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected TI64"

let test_unify_pi () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; codomain = { env = []; body = AtomTy TBool } } in
  let v2 = VPi { explicitness = Explicit; domain = VAtomTy TI64; codomain = { env = []; body = AtomTy TBool } } in
  unify mc [] 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected TI64 in pi domain"

let test_unify_spine () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] } ] } in
  let v2 = VAtomTy TI64 in
  unify mc [] 1 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = clo; _ } ->
      let result = closure_apply mc clo (VAtomTy TBool) in
      (match result with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected constant function")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_rename_id () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] } ] } in
  let v2 = VRigid { lvl = 0; spine = [] } in
  unify mc [] 1 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = clo; _ } ->
      let result = closure_apply mc clo (VAtomTy TI64) in
      Alcotest.(check bool) "rename id" true
        (match result with VAtomTy TI64 -> true | _ -> false)
  | _ -> Alcotest.fail "expected VLam"

let test_unify_rename_fst () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] }; VRigid { lvl = 1; spine = [] } ] } in
  let v2 = VRigid { lvl = 0; spine = [] } in
  unify mc [] 2 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = outer_clo; _ } -> (
      let v_fst = closure_apply mc outer_clo (VAtomTy TI64) in
      match v_fst with
      | VLam { body = inner_clo; _ } ->
          let result = closure_apply mc inner_clo (VAtomTy TBool) in
          Alcotest.(check bool) "rename fst" true
            (match result with VAtomTy TI64 -> true | _ -> false)
      | _ -> Alcotest.fail "expected inner VLam")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_rename_snd () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] }; VRigid { lvl = 1; spine = [] } ] } in
  let v2 = VRigid { lvl = 1; spine = [] } in
  unify mc [] 2 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = outer_clo; _ } -> (
      let v_fst = closure_apply mc outer_clo (VAtomTy TI64) in
      match v_fst with
      | VLam { body = inner_clo; _ } ->
          let result = closure_apply mc inner_clo (VAtomTy TBool) in
          Alcotest.(check bool) "rename snd" true
            (match result with VAtomTy TBool -> true | _ -> false)
      | _ -> Alcotest.fail "expected inner VLam")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_occurs_check () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [] } in
  let v2 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; codomain = { env = []; body = AtomTy TBool } } in
  match unify mc [] 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected occurs check error"

let test_unify_nonlinear_spine () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [
    VRigid { lvl = 2; spine = [] };
    VRigid { lvl = 2; spine = [] }
  ]} in
  let v2 = VAtomTy TI64 in
  match unify mc [] 3 v1 v2 with
  | exception UnifyError NonLinearSpine -> ()
  | exception UnifyError _ -> Alcotest.fail "wrong unify error (expected NonLinearSpine)"
  | () -> Alcotest.fail "expected NonLinearSpine but solved silently"

let test_unify_mismatch () =
  let mc = mc () in
  let v1 = VAtomTy TI64 in
  let v2 = VAtomTy TBool in
  match unify mc [] 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected unify error"

let test_unify_nominal_params () =
  let mc = mc () in
  let nom1 = VNominal { id = 99; name = "Option"; params = [ VAtomTy TI64 ]; constructors = [] } in
  let nom2 = VNominal { id = 99; name = "Option"; params = [ VAtomTy TBool ]; constructors = [] } in
  match unify mc [] 0 nom1 nom2 with
  | exception UnifyError _ -> ()
  | () -> Alcotest.fail "expected unify error for different nominal params"

let () =
  Alcotest.run "core"
    [
      ( "eval",
        [
          Alcotest.test_case "atom" `Quick (check_i64 "atom" 42L "42");
          Alcotest.test_case "lam+ap" `Quick (check_i64 "lam+ap" 7L "(fun x -> x) 7");
          Alcotest.test_case "let" `Quick (check_i64 "let" 5L "let x : I64 = 5 in x");
          Alcotest.test_case "if true" `Quick (check_i64 "if true" 1L "if true then 1 else 2");
          Alcotest.test_case "if false" `Quick (check_i64 "if false" 2L "if false then 1 else 2");
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "proj" `Quick (check_i64 "proj" 42L "(42, true).0");
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "pi" `Quick test_eval_pi;
          Alcotest.test_case "fix" `Quick
            (check_i64 "fix" 0L
               "let rec f : Bool -> I64 = fun x -> if x then 0 else f true in f false");
        ] );
      ( "conv",
        [
          Alcotest.test_case "beta" `Quick
            (check_conv "beta" "(fun (x : I64) -> x) 5" "5");
          Alcotest.test_case "eta" `Quick
            (check_conv "eta"
               "fun (x : I64) -> x"
               "fun (y : I64) -> y");
          Alcotest.test_case "not equal" `Quick
            (check_not_conv "not equal" "I64" "Bool");
        ] );
      ( "neutral",
        [
          Alcotest.test_case "var" `Quick test_neutral_var;
          Alcotest.test_case "ap" `Quick test_neutral_ap;
          Alcotest.test_case "if" `Quick test_neutral_if;
        ] );
      ( "meta",
        [
          Alcotest.test_case "solve" `Quick test_meta_solve;
          Alcotest.test_case "conv" `Quick test_meta_conv;
          Alcotest.test_case "inserted" `Quick test_inserted_meta;
        ] );
      ( "unify",
        [
          Alcotest.test_case "simple" `Quick test_unify_simple;
          Alcotest.test_case "pi" `Quick test_unify_pi;
          Alcotest.test_case "spine" `Quick test_unify_spine;
          Alcotest.test_case "rename id" `Quick test_unify_rename_id;
          Alcotest.test_case "rename fst" `Quick test_unify_rename_fst;
          Alcotest.test_case "rename snd" `Quick test_unify_rename_snd;
          Alcotest.test_case "occurs check" `Quick test_unify_occurs_check;
          Alcotest.test_case "nonlinear spine" `Quick test_unify_nonlinear_spine;
          Alcotest.test_case "nominal params" `Quick test_unify_nominal_params;
          Alcotest.test_case "mismatch" `Quick test_unify_mismatch;
        ] );
    ]
