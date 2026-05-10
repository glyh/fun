open Core_tt.Core
open Core_tt.Nbe
open Core_tt.Unify

let mc () = MetaContext.create ()

let term_eq mc depth t1 t2 =
  let v1 = eval mc [] t1 in
  let v2 = eval mc [] t2 in
  conv mc depth v1 v2

let test_eval_atom () =
  let mc = mc () in
  let v = eval mc [] (Atom (I64 42L)) in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "i64" 42L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_lam_ap () =
  let mc = mc () in
  let id = Lam (Var 0) in
  let t = Ap (id, Atom (I64 7L)) in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "applied" 7L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_let () =
  let mc = mc () in
  let t = Let (AtomTy TI64, Atom (I64 5L), Var 0) in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "let" 5L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_if_true () =
  let mc = mc () in
  let t = If (Atom (Bool true), Atom (I64 1L), Atom (I64 2L)) in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "if true" 1L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_if_false () =
  let mc = mc () in
  let t = If (Atom (Bool false), Atom (I64 1L), Atom (I64 2L)) in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "if false" 2L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_prod () =
  let mc = mc () in
  let t = Prod [ Atom (I64 1L); Atom (Bool true) ] in
  let v = eval mc [] t in
  match v with
  | VProd [ VAtom (I64 1L); VAtom (Bool true) ] -> ()
  | _ -> Alcotest.fail "expected VProd"

let test_eval_proj () =
  let mc = mc () in
  let t = Proj (Prod [ Atom (I64 42L); Atom (Bool true) ], 0) in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "proj0" 42L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_dot () =
  let mc = mc () in
  let s = Struct { con_fields = [];
                   bindings = [ ("x", Public, Atom (I64 99L)); ("y", Public, Atom (Bool true)) ];
                   partial = false } in
  let t = Dot (s, "x") in
  let v = eval mc [] t in
  match v with VAtom (I64 n) -> Alcotest.(check int64) "dot" 99L n | _ -> Alcotest.fail "expected VAtom"

let test_eval_pi () =
  let mc = mc () in
  let t = Pi (AtomTy TI64, AtomTy TBool) in
  let v = eval mc [] t in
  match v with
  | VPi { domain = VAtomTy TI64; _ } -> ()
  | _ -> Alcotest.fail "expected VPi"

let test_quote_atom () =
  let mc = mc () in
  let t = Atom (I64 42L) in
  let v = eval mc [] t in
  let t' = quote mc 0 v in
  Alcotest.(check bool) "round-trip" true (term_eq mc 0 t t')

let test_quote_lam () =
  let mc = mc () in
  let t = Lam (Var 0) in
  let v = eval mc [] t in
  let t' = quote mc 0 v in
  Alcotest.(check bool) "round-trip lam" true (term_eq mc 0 t t')

let test_quote_pi () =
  let mc = mc () in
  let t = Pi (U, Pi (Var 0, Var 1)) in
  let v = eval mc [] t in
  let t' = quote mc 0 v in
  Alcotest.(check bool) "round-trip pi" true (term_eq mc 0 t t')

let test_conv_beta () =
  let mc = mc () in
  let t1 = Ap (Lam (Var 0), AtomTy TI64) in
  let t2 = AtomTy TI64 in
  Alcotest.(check bool) "beta conv" true (term_eq mc 0 t1 t2)

let test_conv_eta () =
  let mc = mc () in
  let f = Lam (Ap (Var 1, Var 0)) in
  let v_f = eval mc [ VRigid { lvl = 0; spine = [] } ] f in
  let v_g = VRigid { lvl = 0; spine = [] } in
  Alcotest.(check bool) "eta conv" true (conv mc 1 v_f v_g)

let test_conv_pi () =
  let mc = mc () in
  let t1 = Pi (U, Pi (Var 0, Var 1)) in
  let t2 = Pi (U, Pi (Var 0, Var 1)) in
  Alcotest.(check bool) "alpha-equiv pi" true (term_eq mc 0 t1 t2)

let test_conv_not_equal () =
  let mc = mc () in
  let v1 = eval mc [] (AtomTy TI64) in
  let v2 = eval mc [] (AtomTy TBool) in
  Alcotest.(check bool) "not equal" false (conv mc 0 v1 v2)

let test_neutral_var () =
  let mc = mc () in
  let v = eval mc [ VRigid { lvl = 0; spine = [] } ] (Var 0) in
  match v with VRigid { lvl = 0; spine = [] } -> () | _ -> Alcotest.fail "expected VRigid"

let test_neutral_ap () =
  let mc = mc () in
  let env = [ VRigid { lvl = 0; spine = [] } ] in
  let v = eval mc env (Ap (Var 0, Atom (I64 1L))) in
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

let test_unify_simple () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [] } in
  let v2 = VAtomTy TI64 in
  unify mc 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected TI64"

let test_unify_pi () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VPi { domain = VFlex { id; spine = [] }; codomain = { env = []; body = AtomTy TBool } } in
  let v2 = VPi { domain = VAtomTy TI64; codomain = { env = []; body = AtomTy TBool } } in
  unify mc 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected TI64 in pi domain"

let test_unify_spine () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] } ] } in
  let v2 = VAtomTy TI64 in
  unify mc 1 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = clo; _ } ->
      let result = closure_apply mc clo (VAtomTy TBool) in
      (match result with VAtomTy TI64 -> () | _ -> Alcotest.fail "expected constant function")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_rename_id () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  (* ?M[x] = x  →  solution should be λx0. x0 (identity) *)
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] } ] } in
  let v2 = VRigid { lvl = 0; spine = [] } in
  unify mc 1 v1 v2;
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
  (* ?M[x, y] = x  →  solution should be λx0. λx1. x0 (project first) *)
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] }; VRigid { lvl = 1; spine = [] } ] } in
  let v2 = VRigid { lvl = 0; spine = [] } in
  unify mc 2 v1 v2;
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
  (* ?M[x, y] = y  →  solution should be λx0. λx1. x1 (project second) *)
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] }; VRigid { lvl = 1; spine = [] } ] } in
  let v2 = VRigid { lvl = 1; spine = [] } in
  unify mc 2 v1 v2;
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
  let v2 = VPi { domain = VFlex { id; spine = [] }; codomain = { env = []; body = AtomTy TBool } } in
  match unify mc 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected occurs check error"

let test_unify_mismatch () =
  let mc = mc () in
  let v1 = VAtomTy TI64 in
  let v2 = VAtomTy TBool in
  match unify mc 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected unify error"

let () =
  Alcotest.run "core"
    [
      ( "eval",
        [
          Alcotest.test_case "atom" `Quick test_eval_atom;
          Alcotest.test_case "lam+ap" `Quick test_eval_lam_ap;
          Alcotest.test_case "let" `Quick test_eval_let;
          Alcotest.test_case "if true" `Quick test_eval_if_true;
          Alcotest.test_case "if false" `Quick test_eval_if_false;
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "proj" `Quick test_eval_proj;
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "pi" `Quick test_eval_pi;
        ] );
      ( "quote",
        [
          Alcotest.test_case "atom" `Quick test_quote_atom;
          Alcotest.test_case "lam" `Quick test_quote_lam;
          Alcotest.test_case "pi" `Quick test_quote_pi;
        ] );
      ( "conv",
        [
          Alcotest.test_case "beta" `Quick test_conv_beta;
          Alcotest.test_case "eta" `Quick test_conv_eta;
          Alcotest.test_case "pi alpha" `Quick test_conv_pi;
          Alcotest.test_case "not equal" `Quick test_conv_not_equal;
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
          Alcotest.test_case "mismatch" `Quick test_unify_mismatch;
        ] );
    ]
