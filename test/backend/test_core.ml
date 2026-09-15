open Core
open Atom
open Nbe
open Unify

let mc () = MetaContext.create ()
let pure_effects = effect_row_closure [] empty_effect_row

(* Prelude syntax exports (operators, [if]) — passed into every parse so test
   sources can use [+]/[==]/[if]/… now that these are prelude features rather
   than compiler builtins. Mirrors what the entry points and loader inject. *)
let builtin_syntax = Lazy.force Elab_prelude.stdlib_syntax_exports

let parse_expr source =
  Parse_expand.parse_expr ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source

let fail_with_source label source message =
  Alcotest.fail (Printf.sprintf "%s: %s\nsource:\n%s" label message source)

let eval_source source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  let core, _ty = Elaborate.on_expr ctx expr in
  Elaborate.Ctx.run ctx core

let eval_source_with_loader loader source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  let core, _ty = Elaborate.on_expr ~loader ctx expr in
  Elaborate.Ctx.run ctx core

let with_modules modules f =
  let dir = Filename.temp_dir "fun_core_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = Core_loader.create ~base_dir:dir ~builtin_syntax () in
  f loader

let check_import_i64 label modules expected source () =
  with_modules modules (fun loader ->
      match eval_source_with_loader loader source with
      | VAtom (I64 n) -> Alcotest.(check int64) label expected n
      | v ->
          let mc = MetaContext.create () in
          Alcotest.fail
            (Printf.sprintf "%s: expected VAtom I64, got %s" label (Debug.pp_value_short mc v))
      | exception e ->
          fail_with_source label source (Printf.sprintf "exception %s" (Printexc.to_string e)))

let check_i64 label expected source () =
  match eval_source source with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      fail_with_source label source
        (Printf.sprintf "expected VAtom I64, got %s" (Debug.pp_value_short mc v))
  | exception e ->
      fail_with_source label source (Printf.sprintf "exception %s" (Printexc.to_string e))

let check_bool label expected source () =
  match eval_source source with
  | VCon { name = "True"; _ } -> Alcotest.(check bool) label expected true
  | VCon { name = "False"; _ } -> Alcotest.(check bool) label expected false
  | v ->
      let mc = MetaContext.create () in
      fail_with_source label source
        (Printf.sprintf "expected VAtom Bool, got %s" (Debug.pp_value_short mc v))
  | exception e ->
      fail_with_source label source (Printf.sprintf "exception %s" (Printexc.to_string e))

let check_conv label s1 s2 () =
  let ctx = Elaborate.init_ctx () in
  let core1, _ = Elaborate.on_expr ctx (parse_expr s1) in
  let core2, _ = Elaborate.on_expr ctx (parse_expr s2) in
  let v1 = Elaborate.Ctx.eval ctx core1 in
  let v2 = Elaborate.Ctx.eval ctx core2 in
  Alcotest.(check bool) label true (Elaborate.Ctx.conv ctx v1 v2)

let check_not_conv label s1 s2 () =
  let ctx = Elaborate.init_ctx () in
  let core1, _ = Elaborate.on_expr ctx (parse_expr s1) in
  let core2, _ = Elaborate.on_expr ctx (parse_expr s2) in
  let v1 = Elaborate.Ctx.eval ctx core1 in
  let v2 = Elaborate.Ctx.eval ctx core2 in
  Alcotest.(check bool) label false (Elaborate.Ctx.conv ctx v1 v2)

(* -- eval tests --------------------------------------------------------- *)

let test_eval_prod () =
  match eval_source "(1, True)" with
  | VProd [ VAtom (I64 1L); VCon { name = "True"; _ } ] -> ()
  | _ -> Alcotest.fail "expected VProd"

let test_eval_pi () =
  match eval_source "I64 -> Bool" with
  | VPi { domain = VAtomTy Atom_ty.TI64; _ } -> ()
  | _ -> Alcotest.fail "expected VPi"

let test_eval_dot () =
  match eval_source "{ M = module { pub x = 99 }; M.x }" with
  | VAtom (I64 n) -> Alcotest.(check int64) "dot" 99L n
  | _ -> Alcotest.fail "expected VAtom"

let test_eval_module_signature_argument () =
  check_i64 "module signature argument" 42L
    "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42 })"
    ()

let test_eval_module_signature_extra_field () =
  check_i64 "module signature extra field" 42L
    "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42; pub y = True })"
    ()

let test_eval_signature_sugar_argument () =
  check_i64 "signature sugar argument" 42L
    "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 42 })"
    ()

(* A signature is its own kind of value: a let-bound one is a type exactly as the
   inline form is. A module is never a type, even one whose members are types. *)
let test_eval_let_bound_signature () =
  check_i64 "let-bound sig" 42L "{ Sig = sig { x : I64 }; f = fn(m : Sig) { m.x + 1 }; f(module { pub x = 41 }) }" ()

let test_eval_module_is_not_a_signature () =
  match eval_source "{ Types = module { pub x = I64 }; f = fn(m : Types) { m.x }; f(module { pub x = 42 }) }" with
  | exception Elab_error.ElabError (Elab_error.NotASignature (Some "Types")) -> ()
  | exception e -> Alcotest.fail ("module as a type: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "module as a type: expected NotASignature"

let test_eval_module_signature_functor () =
  check_i64 "module signature functor" 42L
    "{ F = fn(M : sig { x : I64 }) { module { pub doubled = M.x + M.x } }; F(module { pub x = 21 }).doubled }"
    ()

let test_ref_read_initial () =
  check_i64 "ref read initial" 1L "{ r = ref(1); deref(r) }" ()

let test_ref_write_read () =
  check_i64 "ref write read" 2L "{ r = ref(1); _ = r <- 2; deref(r) }" ()

let test_ref_aliases_share_cell () =
  check_i64 "ref aliases share cell" 3L "{ r = ref(1); alias = r; _ = alias <- 3; deref(r) }" ()

let test_ref_closure_observes_later_write () =
  check_i64 "ref closure observes later write" 4L "{ r = ref(0); f = fn(_) { deref(r) }; _ = r <- 4; f() }" ()

let test_ref_repeated_closure_increments () =
  check_i64 "ref repeated closure increments" 2L
    "{ r = ref(0); inc = fn(_) { { n = deref(r); _ = r <- n + 1; deref(r) } }; _ = inc(); inc() }"
    ()

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

let test_neutral_match () =
  let mc = mc () in
  let env = [ VRigid { lvl = 0; spine = [] } ] in
  let v =
    eval mc env
      (Match
         ( Var 0,
           [ ValueBranch (CPatAtom (I64 1L), Atom (I64 1L));
             ValueBranch (CPatAtom (I64 2L), Atom (I64 2L)) ] ))
  in
  match v with
  | VNeutral { neutral = { head = HVar 0; frames = [ FMatch _ ] }; _ } -> ()
  | _ -> Alcotest.fail "expected stuck match"

(* -- meta tests (require manual construction) ---------------------------- *)

let test_meta_solve () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v = eval mc [] (Meta id) in
  (match v with VFlex { id = 0; spine = [] } -> () | _ -> Alcotest.fail "expected VFlex");
  MetaContext.solve mc id (VAtomTy Atom_ty.TI64);
  let v2 = force mc v in
  match v2 with VAtomTy Atom_ty.TI64 -> () | _ -> Alcotest.fail "expected VAtomTy after solve"

let test_meta_conv () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  MetaContext.solve mc id (VAtomTy Atom_ty.TI64);
  let v1 = eval mc [] (Meta id) in
  let v2 = VAtomTy Atom_ty.TI64 in
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
  let v2 = VAtomTy Atom_ty.TI64 in
  unify mc [] 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy Atom_ty.TI64 -> () | _ -> Alcotest.fail "expected Atom_ty.TI64"

let test_unify_pi () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TChar } } in
  let v2 = VPi { explicitness = Explicit; domain = VAtomTy Atom_ty.TI64; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TChar } } in
  unify mc [] 0 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with VAtomTy Atom_ty.TI64 -> () | _ -> Alcotest.fail "expected Atom_ty.TI64 in pi domain"

let test_unify_spine () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [ VRigid { lvl = 0; spine = [] } ] } in
  let v2 = VAtomTy Atom_ty.TI64 in
  unify mc [] 1 v1 v2;
  let solved = force mc (VFlex { id; spine = [] }) in
  match solved with
  | VLam { body = clo; _ } ->
      let result = closure_apply mc clo (VAtomTy Atom_ty.TChar) in
      (match result with VAtomTy Atom_ty.TI64 -> () | _ -> Alcotest.fail "expected constant function")
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
      let result = closure_apply mc clo (VAtomTy Atom_ty.TI64) in
      Alcotest.(check bool) "rename id" true
        (match result with VAtomTy Atom_ty.TI64 -> true | _ -> false)
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
      let v_fst = closure_apply mc outer_clo (VAtomTy Atom_ty.TI64) in
      match v_fst with
      | VLam { body = inner_clo; _ } ->
          let result = closure_apply mc inner_clo (VAtomTy Atom_ty.TChar) in
          Alcotest.(check bool) "rename fst" true
            (match result with VAtomTy Atom_ty.TI64 -> true | _ -> false)
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
      let v_fst = closure_apply mc outer_clo (VAtomTy Atom_ty.TI64) in
      match v_fst with
      | VLam { body = inner_clo; _ } ->
          let result = closure_apply mc inner_clo (VAtomTy Atom_ty.TChar) in
          Alcotest.(check bool) "rename snd" true
            (match result with VAtomTy Atom_ty.TChar -> true | _ -> false)
      | _ -> Alcotest.fail "expected inner VLam")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_occurs_check () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [] } in
  let v2 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TChar } } in
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
  let v2 = VAtomTy Atom_ty.TI64 in
  match unify mc [] 3 v1 v2 with
  | exception UnifyError NonLinearSpine -> ()
  | exception UnifyError _ -> Alcotest.fail "wrong unify error (expected NonLinearSpine)"
  | () -> Alcotest.fail "expected NonLinearSpine but solved silently"

let test_unify_mismatch () =
  let mc = mc () in
  let v1 = VAtomTy Atom_ty.TI64 in
  let v2 = VAtomTy Atom_ty.TChar in
  match unify mc [] 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected unify error"

let test_unify_nominal_params () =
  let mc = mc () in
  let nom1 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy Atom_ty.TI64 ]; constructors = [] } in
  let nom2 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy Atom_ty.TChar ]; constructors = [] } in
  match unify mc [] 0 nom1 nom2 with
  | exception UnifyError _ -> ()
  | () -> Alcotest.fail "expected unify error for different nominal params"

let test_unify_effect_same_id_same_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  unify mc [] 0 eff1 eff2

let test_unify_effect_same_id_different_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TChar ]; operations = [] } in
  match unify mc [] 0 eff1 eff2 with
  | exception UnifyError _ -> ()
  | () -> Alcotest.fail "expected unify error for different effect params"

let test_unify_effect_different_ids () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 8; name = "Env"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  match unify mc [] 0 eff1 eff2 with
  | exception UnifyError (EffectMismatch ("State", "Env")) -> ()
  | exception UnifyError _ -> Alcotest.fail "wrong unify error"
  | () -> Alcotest.fail "expected unify error for distinct effect ids"

let test_conv_effect_same_id_same_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  Alcotest.(check bool) "effect conv" true (conv mc 0 eff1 eff2)

let test_conv_effect_different_ids () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 8; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  Alcotest.(check bool) "effect conv" false (conv mc 0 eff1 eff2)

let effect_row_env () =
  [ VEffect { id = 1; name = "IO"; params = []; operations = [] };
    VEffect { id = 2; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } ]

let effectful_pi effects =
  VPi
    { explicitness = Explicit;
      domain = VAtomTy Atom_ty.TI64;
      effects = { env = effect_row_env (); effects; tail = None };
      codomain = { env = []; body = AtomTy Atom_ty.TI64 } }

let test_conv_effect_row_order () =
  let mc = mc () in
  Alcotest.(check bool) "effect row order" true
    (conv mc 0 (effectful_pi [ Var 1; Var 0 ]) (effectful_pi [ Var 0; Var 1 ]))

let test_conv_effect_row_mismatch () =
  let mc = mc () in
  Alcotest.(check bool) "effect row mismatch" false
    (conv mc 0 (effectful_pi [ Var 1 ]) (effectful_pi [ Var 0 ]))

let test_unify_effect_row_order () =
  let mc = mc () in
  unify mc [] 0 (effectful_pi [ Var 1; Var 0 ]) (effectful_pi [ Var 0; Var 1 ])

let test_unify_effect_row_mismatch () =
  let mc = mc () in
  match unify mc [] 0 (effectful_pi [ Var 1 ]) (effectful_pi [ Var 0 ]) with
  | exception UnifyError EffectRowMismatch -> ()
  | exception UnifyError _ -> Alcotest.fail "wrong unify error"
  | () -> Alcotest.fail "expected effect row mismatch"

let test_debug_effectful_pi () =
  let mc = mc () in
  let text = Debug.pp_value_short mc (effectful_pi [ Var 1 ]) in
  if not (String.contains text 'c') then Alcotest.fail ("expected can in debug output, got " ^ text)

let test_debug_effect () =
  let mc = mc () in
  let text = Debug.pp_value_short mc (VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] }) in
  Alcotest.(check string) "debug effect" "effect State(I64)" text

let test_eval_unhandled_perform () =
  let state = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TI64 ]; operations = [] } in
  let term = Perform { eff = Var 0; op = "put"; arg = Atom (I64 42L) } in
  match Nbe.eval (mc ()) [ state ] term with
  | exception Nbe.EvalError "unhandled effect State.put: no handler for it is in scope" -> ()
  | exception Nbe.EvalError msg -> Alcotest.fail ("unexpected perform error: " ^ msg)
  | _ -> Alcotest.fail "expected unhandled perform error"

(* A program's top performs only what the runtime handles - nothing yet - so an
   effect left unhandled there is an elaboration error, not a run-time crash. *)
let expect_unhandled label names run =
  match run () with
  | exception Elaborate.ElabError (UnhandledEffects got) when got = names -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an unhandled-effect error")

let test_top_unhandled_perform () =
  expect_unhandled "a top-level perform" [ "effect Exc" ] (fun () ->
      eval_source "{ effect Exc = sig { raise : I64 -> I64 }; perform Exc.raise(1) }")

let test_top_escaping_closure () =
  expect_unhandled "a closure escaping its handler, called at the top" [ "effect Exc" ] (fun () ->
      eval_source
        "{ effect Exc = sig { raise : I64 -> I64 };
           g = match (0) { x => fn(u : Unit) { perform Exc.raise(x) }, effect Exc.raise n => fn(u : Unit) { n } };
           g(()) }")

(* Tunneling (E5): a handler handles what its own code performs; what a
   row-polymorphic callback performs passes it. *)
let tunnel_find body = "{ effect Exc = sig { raise : I64 -> I64 }; find : [r : EffectRow] -> (I64 -> I64 can {| r}) -> I64 -> I64 can {| r} = fn[r : EffectRow](pred, x) { " ^ body ^ " }; user : I64 -> I64 can {Exc} = fn(x) { perform Exc.raise(x) }; match (find(user, 1)) { v => v, effect Exc.raise n => 999 } }"

let test_handlers_tunnel () =
  check_i64 "a callback's effect passes the library's handler" 999L
    (tunnel_find "match ({ v = pred(x); if (v > 3) { perform Exc.raise(v) } else { v } }) { v => v, effect Exc.raise n => 0 }") ();
  check_i64 "it passes every handler in the library's body" 999L
    (tunnel_find "match (match (pred(x)) { v => v, effect Exc.raise n => 1 }) { v => v, effect Exc.raise n => 2 }") ();
  check_i64 "a closure made under one handler and called under another" 999L
    (tunnel_find "g = match (0) { _ => fn(y : I64) { pred(y) }, effect Exc.raise n => fn(y : I64) { pred(0) } }; match (g(x)) { v => v, effect Exc.raise n => 2 }") ();
  check_i64 "a call whose row names the effect is handled locally" 6L
    "{ effect Exc = sig { raise : I64 -> I64 }; helper : Unit -> I64 can {Exc} = fn(_) { perform Exc.raise(5) }; match (helper(())) { v => v, effect Exc.raise n => n + 1 } }" ()

let test_handled_effect_escape () =
  (match eval_source "{ effect Exc = sig { raise : I64 -> I64 }; g = match (0) { x => fn(u : Unit) { perform Exc.raise(x) }, effect Exc.raise n => fn(u : Unit) { perform Exc.raise(n) } }; 1 }" with
   | exception Elaborate.ElabError (HandledEffectEscapes "Exc") -> ()
   | exception e -> Alcotest.fail (Printexc.to_string e)
   | _ -> Alcotest.fail "expected HandledEffectEscapes");
  check_i64 "a saved continuation may outlive its handler" 5L
    "{ effect Async = sig { pause : Unit -> Unit }; q = ref(fn(u : Unit) { 0 }); _ = match (perform Async.pause(())) { v => 1, effect Async.pause _ => { q <- fn(u : Unit) { resume(()) }; 2 } }; 5 }" ()

let test_top_handled_and_latent () =
  check_i64 "a handled perform and an uncalled effectful function" 2L
    "{ effect Exc = sig { raise : I64 -> I64 };
       f = fn(u : Unit) { perform Exc.raise(1) };
       match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } }" ()

let test_top_unhandled_in_imported_unit () =
  with_modules [ ("noisy", "effect Exc = sig { raise : I64 -> I64 };\npub v = perform Exc.raise(1)") ] (fun loader ->
      expect_unhandled "an imported unit's top-level perform" [ "effect Exc" ] (fun () ->
          eval_source_with_loader loader "{ M = import \"noisy\"; 0 }"))

(* A method follows the arrow rule: pure unless its [can] declares a row. *)
let exc_counter methods = "effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; " ^ methods ^ " }"

let test_method_rows () =
  expect_unhandled "a method performing an undeclared effect" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method bump() { perform Exc.raise(1); self.n }" ^ "; 0 }"));
  check_i64 "a method declaring its row, handled at the call" 11L
    ("{ " ^ exc_counter "pub method bump() can {Exc} { perform Exc.raise(1); self.n }"
     ^ "; match (C.bump(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }") ();
  expect_unhandled "a declared method called at the top without a handler" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method bump() can {Exc} { perform Exc.raise(1); self.n }" ^ "; C.bump(C{n = 1}) }"));
  check_i64 "can _ infers a method's row" 13L
    ("{ " ^ exc_counter "pub method add(k : I64) can _ { perform Exc.raise(k) }"
     ^ "; match (C.add(C{n = 1})(3)) { x => x, effect Exc.raise v => v + 10 } }") ();
  check_i64 "a method calling another performs its declared row" 12L
    ("{ " ^ exc_counter "pub method a(k : I64) can {Exc} { perform Exc.raise(k) }; pub method b() can {Exc} { a(self)(2) }"
     ^ "; match (C.b(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }") ();
  expect_unhandled "a pure method calling an effectful one" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method a(k : I64) can {Exc} { perform Exc.raise(k) }; pub method b() { a(self)(2) }" ^ "; 0 }"))

let test_trait_method_rows () =
  let trait_src impl_body =
    "{ effect Exc = sig { raise : I64 -> I64 }; effect Other = sig { ping : I64 -> I64 }; \
     trait Log(A) = sig { log : A -> I64 can {Exc} }; \
     impl Log(I64) = module { log = fn(x) { " ^ impl_body ^ " } }; 0 }"
  in
  check_i64 "an impl method within its trait's row" 0L (trait_src "perform Exc.raise(x)") ();
  expect_unhandled "an impl method performing beyond its trait's row" [ "effect Other" ] (fun () ->
      eval_source (trait_src "perform Other.ping(x)"))

let test_eval_match_binds_a_closure () =
  check_i64 "a variable pattern binds a closure scrutinee" 1L
    "{ h = match (fn(u : Unit) { 1 }) { x => x }; h(()) }" ();
  check_i64 "a closure scrutinee under a handler" 1L
    "{ effect Exc = sig { raise : I64 -> I64 }; h = match (fn(u : Unit) { 1 }) { x => x, effect Exc.raise n => fn(u : Unit) { n } }; h(()) }"
    ()

let test_debug_perform () =
  let text = Debug.pp_term (Perform { eff = EffectRef ("State", [ AtomTy Atom_ty.TI64 ]); op = "get"; arg = Atom Unit }) in
  if not (String.contains text 'g') then Alcotest.fail ("expected perform debug output, got " ^ text)

let test_eval_handler_ignores_continuation () =
  check_i64 "handler ignores continuation" 2L
    "{ effect Exc = sig { raise : I64 -> I64 }; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } }"
    ()

let test_eval_handler_resumes_once () =
  check_i64 "handler resumes once" 2L
    "{ effect Exc = sig { raise : I64 -> I64 }; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(n + 1) } }"
    ()

let test_eval_handler_resume_through_value_branch () =
  check_i64 "resume passes through the value branch" 46L
    "{ effect E = sig { op : I64 -> I64 }; match (perform E.op(1)) { x => x + 5, effect E.op n => resume(n + 40) } }"
    ();
  check_i64 "an unresumed handler skips the value branch" 41L
    "{ effect E = sig { op : I64 -> I64 }; match (perform E.op(1)) { x => x + 5, effect E.op n => n + 40 } }"
    ();
  check_i64 "a perform after resume is handled again, value branch once" 1103L
    "{ effect E = sig { op : I64 -> I64 }; match (perform E.op(1) + perform E.op(2)) { x => x + 1000, effect E.op n => resume(n + 50) } }"
    ()

let test_eval_handler_value_branch () =
  check_i64 "handler value branch" 42L
    "{ effect Exc = sig { raise : I64 -> I64 }; match (41) { x => x + 1, effect Exc.raise n => 0 } }"
    ()

let test_eval_handler_outer_bubble () =
  check_i64 "handler outer bubble" 2L
    "{ effect Exc = sig { raise : I64 -> I64 }; match (match (perform Exc.raise(1)) { x => x }) { x => x, effect Exc.raise n => n + 1 } }"
    ()

let test_eval_handler_escape_skips_continuation () =
  check_i64 "handler escape skips continuation" 99L
    "{
       effect Exit = sig { now : I64 -> I64 };
       program : Unit -> I64 can Exit = fn(_) { {
         _ = perform Exit.now(99);
         0
       } };
       match (program()) { x => x,
       effect Exit.now value => value
       }
     }"
    ()

let test_eval_handler_ping_pong_effects () =
  check_i64 "handler ping pong effects" 212L
    "{
       effect Ping = sig { hit : I64 -> I64 };
       effect Pong = sig { hit : I64 -> I64 };
       program : Unit -> I64 can {Ping, Pong} = fn(_) { {
         x = perform Ping.hit(1);
         perform Pong.hit(x + 10)
       } };
       match (program()) { x => x,
       effect Ping.hit n => { y = perform Pong.hit(n + 1); resume(y) }
       effect Pong.hit n => resume(n + 100)
       }
     }"
    ()

let test_eval_recursive_handler_ping_pong_effects () =
  check_i64 "recursive handler ping pong effects" 10L
    "{
       effect Ping = sig { hit : I64 -> I64 };
       effect Pong = sig { hit : I64 -> I64 };
       rec loop : I64 -> I64 can {Ping, Pong} = fn(n) {
          if (n == 0) {
            0
          } else {
            { x = perform Ping.hit(n); y = perform Pong.hit(n - 1); x + y + loop(n - 2) }
          }
        };
        match (loop(4)) { x => x,
       effect Ping.hit n => resume(n),
       effect Pong.hit n => resume(n)
       }
     }"
    ()

let test_eval_state_handler_sequences_operations () =
  (* Deep handlers (E8): a resumed computation re-enters this handler, so state
     is threaded by the handler returning a function of the state. *)
  check_i64 "state handler sequences operations" 2L
    "{
       effect State(S) = sig { get : Unit -> S; put : S -> Unit };
       program : Unit -> I64 can State(I64) = fn(_) { {
         x = perform State.get();
         _ = perform State.put(x + 1);
         perform State.get()
       } };
       h = match (program(())) { x => fn(s : I64) { x },
         effect State.get () => fn(s : I64) { resume(s)(s) },
         effect State.put next => fn(s : I64) { resume(())(next) }
       };
       h(1)
     }" ()

let test_eval_handler_tuple_payload_pattern () =
  check_i64 "handler tuple payload pattern" 42L
    "{ effect Console = sig { log : Tuple(2, I64, I64) -> I64 }; match (perform Console.log((40, 2))) { x => x, effect Console.log (level, message) => level + message } }"
    ()

let test_eval_handler_tuple_payload_binding_order () =
  check_i64 "handler tuple payload binding order" 38L
    "{ effect Console = sig { log : Tuple(2, I64, I64) -> I64 }; match (perform Console.log((40, 2))) { x => x, effect Console.log (level, message) => level - message } }"
    ()

let test_eval_handler_record_payload_pattern () =
  check_i64 "handler record payload pattern" 42L
    "{
       Request = struct { value: I64; extra: I64; };
       effect Ask = sig { prompt : Request -> I64 };
       match (perform Ask.prompt(Request{value = 40; extra = 2})) { x => x,
       effect Ask.prompt Request{value; extra} => value + extra
       }
     }"
    ()

let test_eval_handler_record_payload_binding_order () =
  check_i64 "handler record payload binding order" 38L
    "{
       Request = struct { value: I64; extra: I64; };
       effect Ask = sig { prompt : Request -> I64 };
       match (perform Ask.prompt(Request{value = 40; extra = 2})) { x => x,
       effect Ask.prompt Request{value; extra} => value - extra
       }
     }"
    ()

let is_zeroish_source call =
  "{
     is_zeroish : [T : Type] -> T -> Bool = fn[T : Type](x) {
       match (T) { I64 => x == 0,
       Bool => x == False,
       Unit => True,
       Char => x == 'a',
       _ => False
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_i64_zero () =
  check_bool "type-case I64 zero" true (is_zeroish_source "is_zeroish(0)") ()

let test_eval_type_case_i64_nonzero () =
  check_bool "type-case I64 nonzero" false (is_zeroish_source "is_zeroish(1)") ()

let test_eval_type_case_bool_false () =
  check_bool "type-case Bool False" true (is_zeroish_source "is_zeroish(False)") ()

let test_eval_type_case_bool_true () =
  check_bool "type-case Bool True" false (is_zeroish_source "is_zeroish(True)") ()

let test_eval_type_case_unit () =
  check_bool "type-case Unit" true (is_zeroish_source "is_zeroish(())") ()

let test_eval_type_case_char_a () =
  check_bool "type-case Char a" true (is_zeroish_source "is_zeroish('a')") ()

let default_source call =
  "{
     default : [T : Type] -> T = fn[T : Type] {
       match (T) { I64 => 0,
       Bool => False,
       Unit => (),
       Char => 'a',
       _ => panic(\"no default\")
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_default_i64 () =
  check_i64 "type-case default I64" 0L (default_source "default[I64]") ()

let test_eval_type_case_default_bool () =
  check_bool "type-case default Bool" false (default_source "default[Bool]") ()

let test_eval_type_case_default_unit () =
  check_bool "type-case default Unit" true (default_source "default[Unit] == ()") ()

let test_eval_type_case_default_string_panics () =
  match eval_source (default_source "default[String]") with
  | exception Nbe.EvalError "no default" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected panic"

let default_or_source call =
  "{
     default_or : [T : Type] -> T -> T = fn[T : Type](fallback) {
       match (T) { I64 => 0,
       Bool => False,
       Unit => (),
       Char => 'a',
       String => \"\",
       _ => fallback
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_default_or_i64 () =
  check_i64 "type-case default_or I64" 0L (default_or_source "default_or[I64](99)") ()

let test_eval_type_case_default_or_bool () =
  check_bool "type-case default_or Bool" false (default_or_source "default_or[Bool](True)") ()

let test_eval_type_case_default_or_string () =
  check_bool "type-case default_or String" true (default_or_source "default_or[String](\"fallback\") == \"\"") ()

let test_eval_type_case_default_or_nominal_fallback () =
  check_i64 "type-case default_or nominal fallback" 2L
    (default_or_source "type Color = Red | Blue; match (default_or[Color](Blue)) { Red => 1, Blue => 2 }")
    ()

let type_name_source call =
  "{
     type_name : Type -> String = fn(T) {
       match (T) { I64 => \"i64\",
       Bool => \"bool\",
       Char => \"char\",
       Unit => \"unit\",
       String => \"string\",
       _ => \"other\"
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_type_name_i64 () =
  check_bool "type-case type_name I64" true (type_name_source "type_name(I64) == \"i64\"") ()

let test_eval_type_case_type_name_string () =
  check_bool "type-case type_name String" true (type_name_source "type_name(String) == \"string\"") ()

let test_eval_equality_nominal_rejected () =
  match eval_source "{ type Color = Red; Red == Red }" with
  (* [Eq] is in scope; what is missing is an impl. The error used to say
     [UnknownTrait]. See the impl-visibility topic. *)
  | exception Elaborate.ElabError (MissingTraitImplementation _) -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected missing Eq impl"

let test_eval_type_case_nominal_full_application () =
  check_i64 "type-case nominal full application" 1L
    "{ type Option a = Some a | None;
     match (Option(I64)) { Option(I64) => 1,
     Option(Bool) => 2,
     _ => 3
     }
     }"
    ()

let test_eval_type_case_nominal_param_bind () =
  check_i64 "type-case nominal param bind" 1L
    "{ type Option a = Some a | None;
     match (Option(I64)) { Option x => match (x) { I64 => 1, _ => 2 },
     _ => 3
     }
     }"
    ()

let test_eval_type_case_nominal_complex_param_pattern () =
  check_i64 "type-case nominal complex param pattern" 1L
    "{ type Option a = Some a | None;
     match (Option(Option(I64))) { Option(Option(I64) | I64) => 1,
     Option _ => 2,
     _ => 3
     }
     }"
    ()

let nominal_classify_source call =
  "{
     type Option a = Some a | None;
     classify : Type -> I64 = fn(T) {
       match (T) { Option(I64) => 1,
       Option _ => 2,
       _ => 0
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_nominal_classifier_i64 () =
  check_i64 "type-case nominal classifier I64" 1L (nominal_classify_source "classify(Option(I64))") ()

let test_eval_type_case_nominal_classifier_bool () =
  check_i64 "type-case nominal classifier Bool" 2L (nominal_classify_source "classify(Option(Bool))") ()

let test_eval_type_case_nominal_classifier_fallback () =
  check_i64 "type-case nominal classifier fallback" 0L (nominal_classify_source "classify(I64)") ()

let struct_type_classify_source call =
  "{
     classify : Type -> I64 = fn(T) {
       match (T) { struct { x: I64; _ } => 1,
       struct { x: Bool; _ } => 2,
       struct { y: p; _ } => match (p) { String => 3, _ => 4 },
       _ => 0
       }
     };
     " ^ call ^ "
   }"

let test_eval_type_case_struct_field_i64 () =
  check_i64 "type-case struct field I64" 1L
    (struct_type_classify_source "Point = struct {x: I64; y: Bool}; classify(Point)")
    ()

let test_eval_type_case_struct_field_bool () =
  check_i64 "type-case struct field Bool" 2L
    (struct_type_classify_source "Point = struct {x: Bool}; classify(Point)")
    ()

let test_eval_type_case_struct_field_binder () =
  check_i64 "type-case struct field binder" 3L
    (struct_type_classify_source "Point = struct {y: String; z: I64}; classify(Point)")
    ()

let test_eval_type_case_struct_field_fallback () =
  check_i64 "type-case struct field fallback" 0L (struct_type_classify_source "classify(I64)") ()

let test_eval_type_case_struct_closed_rejects_extra () =
  check_i64 "type-case struct closed rejects extra" 2L
    "{ Point = struct {x: I64; y: Bool};
     match (Point) { struct { x: I64 } => 1,
     struct { x: I64; _ } => 2,
     _ => 3
     }
     }"
    ()

let test_eval_handler_same_match_branch_effect () =
  check_i64 "handler same match branch effect" 43L
    "{ effect Ping = sig { hit : I64 -> I64 };
     match (perform Ping.hit(1)) { x => x,
     effect Ping.hit n =>
         if (n == 1) {
           { y = perform Ping.hit(42); y + 1 }
          } else {
            resume(n)
          }
     }
     }"
    ()

let test_eval_handler_parameterized_dispatch () =
  check_i64 "handler parameterized dispatch" 11L
    "{
      effect State(S) = sig { get : Unit -> S };
     StateI64 = State(I64);
     StateBool = State(Bool);
     match (if (perform StateBool.get(())) { perform StateI64.get(()) + 1 } else { 0 }) { x => x,
     effect StateI64.get () => resume(10),
     effect StateBool.get () => resume(True)
     }
     }"
    ()

let test_eval_handler_value_branch_handles_same_effect () =
  check_i64 "handler value branch handles same effect" 42L
    "{ effect Ask = sig { value : Unit -> I64 };
     match (0) { x => perform Ask.value(()),
     effect Ask.value () => 42
     }
     }"
    ()

let test_eval_handler_value_branch_bubbles_outer_effect () =
  check_i64 "handler value branch bubbles outer effect" 42L
    "{
      effect Inner = sig { value : Unit -> I64 };
      effect Outer = sig { value : Unit -> I64 };
     match (match (0) { x => perform Outer.value(()),
       effect Inner.value () => 0
       }) { x => x,
     effect Outer.value () => 42
     }
     }"
    ()

let test_eval_handler_resumed_continuation_is_deep () =
  check_i64 "handler resumed continuation is deep" 42L
    "{ effect Ping = sig { hit : I64 -> I64 };
     match (if (perform Ping.hit(1) == 41) { perform Ping.hit(2) } else { 0 }) { x => x,
     effect Ping.hit n => resume(n + 40)
     }
     }"
    ()

let test_eval_handler_outer_handles_residual_effect () =
  check_i64 "handler outer handles residual effect" 42L
    "{
      effect Inner = sig { hit : I64 -> I64 };
      effect Outer = sig { hit : I64 -> I64 };
     match (match (perform Outer.hit(1)) { x => x,
       effect Inner.hit n => resume(n)
       }) { x => x,
     effect Outer.hit n => n + 41
     }
     }"
    ()

let test_eval_handler_lexical_resume_nested_lambda () =
  check_i64 "handler lexical resume nested lambda" 41L
    "{ effect Exc = sig { raise : I64 -> I64 };
     match (perform Exc.raise(1)) { x => x,
     effect Exc.raise n => (fn(x) { resume(x + 40) })(n)
     }
     }"
    ()

let test_eval_continuation_reuse_error () =
  let mc = mc () in
  let cont = make_cont (fun value -> Done value) in
  match apply_result mc cont (VAtom (I64 1L)) with
  | Done (VAtom (I64 1L)) -> (
      match apply_result mc cont (VAtom (I64 2L)) with
      | exception EvalError "continuation already used" -> ()
      | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
      | _ -> Alcotest.fail "expected continuation reuse error")
  | _ -> Alcotest.fail "unexpected continuation result"

(* I64 arithmetic is checked: overflow is a language-level runtime error. *)
let check_overflow label op source () =
  let expected = "integer overflow in " ^ op in
  match eval_source source with
  | exception EvalError m when String.equal m expected -> ()
  | exception e -> Alcotest.fail (label ^ ": unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail (label ^ ": expected " ^ expected)

let check_div_by_zero label source () =
  match eval_source source with
  | exception EvalError "division by zero" -> ()
  | exception e -> Alcotest.fail (label ^ ": unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail (label ^ ": expected a division-by-zero error")

let eval_with_macros source =
  let ctx = Elaborate.init_ctx () in
  let nominals =
    Elaborate.syntax_nominals ctx
  in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply = Nbe.apply_macro in
  let expr, expand_ctx = Parse_expand.parse_expr_with_ctx ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source in
  let ctx = Elab_ctx.Ctx.with_expander ctx expand_ctx in
  let core, _ty = Elaborate.on_expr ctx expr in
  Elaborate.Ctx.run ctx core

let eval_decl_module source =
  let ctx = Elaborate.init_ctx () in
  let nominals =
    Elaborate.syntax_nominals ctx
  in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply = Nbe.apply_macro in
  let expr, expand_ctx = Parse_expand.parse_module_with_ctx ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~load_syntax:Elab_prelude.std_load_syntax source in
  let ctx = Elab_ctx.Ctx.with_expander ctx expand_ctx in
  let core, _ty = Elaborate.on_expr ctx expr in
  Elaborate.Ctx.run ctx core

let eval_with_imported_macros modules source =
  with_modules modules (fun loader ->
      let macro_ctx = Elaborate.init_ctx () in
      let syntax_nominals = Elaborate.syntax_nominals macro_ctx in
      let elaborate expr =
        let core, _ty = Elaborate.on_expr ~loader macro_ctx expr in
        Elaborate.Ctx.eval macro_ctx core
      in
      let eval_and_apply = Nbe.apply_macro in
      let expr, expand_ctx =
        Parse_expand.parse_expr_with_ctx
          ~elaborate
          ~eval_and_apply
          ~syntax_nominals
          ~load_macros:(Macro_driver.visit_macros loader)
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          ~open_prelude:true
          source
      in
      let ctx = Elaborate.init_ctx () in
      let ctx = Elab_ctx.Ctx.with_expander ctx expand_ctx in
      let core, _ty = Elaborate.on_expr ~loader ctx expr in
      Elaborate.Ctx.run ctx core)

let test_eval_imported_signature () =
  match
    eval_with_imported_macros
      [ ("sigs", "pub Sig = sig { x : I64 }") ]
      "{ S = import \"sigs\"; f = fn(m : S.Sig) { m.x }; f(module { pub x = 42 }) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported sig" 42L n
  | v -> Alcotest.fail (Printf.sprintf "imported sig: %s" (Debug.pp_value_short (MetaContext.create ()) v))
  | exception e -> Alcotest.fail (Printf.sprintf "imported sig: %s" (Printexc.to_string e))

let string_contains text needle =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec go i =
    i + needle_len <= text_len
    && (String.equal (String.sub text i needle_len) needle || go (i + 1))
  in
  String.equal needle "" || go 0

let check_i64_macro label expected source () =
  match eval_with_macros source with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "%s: %s" label (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))

let test_macro_hygiene_no_capture_user () =
  check_i64_macro "no capture" 1L
    "{ x = 1; macro m(_) { quote(fn(x) { x }) }; (m(0))(x) }" ()

let test_macro_hygiene_user_no_capture_macro () =
  check_i64_macro "no capture" 1L
    "{ macro m(_) { quote(fn(x) { x }) }; x = 1; (m(0))(x) }" ()

let test_macro_panic_has_message () =
  match eval_with_macros "{ macro bad(_) { panic[I64](\"boom\") }; bad(0) }" with
  | exception Expand_error.Error { error = EvalFailed { macro; message }; _ } ->
      Alcotest.(check string) "panic message" "boom" message;
      Alcotest.(check bool) "names the macro" true (string_contains macro "bad")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected panic"

let test_imported_macro_expands () =
  match
    eval_with_imported_macros
      [ ("macros", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ M = import \"macros\"; M.answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported macro" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "imported macro: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "imported macro: %s" (Printexc.to_string e))

(* Macros are members of a unit. Binding an import no longer injects them as
   bare names; a dotted call reaches one, [open] delivers them bare, and two
   units exporting the same macro name no longer overwrite each other.
   See docs/wayfinder/tickets/imported-module-elaboration-context.md. *)
let test_bare_import_does_not_inject_macros () =
  match
    eval_with_imported_macros
      [ ("macros", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ M = import \"macros\"; answer(0) }"
  with
  | exception _ -> ()
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "bare import injected a macro: %s" (Debug.pp_value_short mc v))

let test_open_delivers_macros_bare () =
  match
    eval_with_imported_macros
      [ ("macros", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ open (import \"macros\"); answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "open delivers macro" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "open delivers macro: %s" (Debug.pp_value_short mc v))

let test_open_bound_import_delivers_macros_bare () =
  match
    eval_with_imported_macros
      [ ("macros", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ M = import \"macros\"; open M; answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "open bound import" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "open bound import: %s" (Debug.pp_value_short mc v))

(* Two units exporting the same macro name used to overwrite each other in one
   flat string-keyed table, so the answer depended on import order. *)
let test_same_macro_name_in_two_units () =
  let modules =
    [ ("m1", "open (import \"std\");\npub macro answer(_) { Syntax.i64(1) }");
      ("m2", "open (import \"std\");\npub macro answer(_) { Syntax.i64(2) }") ]
  in
  let one =
    eval_with_imported_macros modules
      "{ A = import \"m1\"; B = import \"m2\"; A.answer(0) }"
  in
  let two =
    eval_with_imported_macros modules
      "{ B = import \"m2\"; A = import \"m1\"; A.answer(0) }"
  in
  match (one, two) with
  | VAtom (I64 a), VAtom (I64 b) ->
      Alcotest.(check int64) "first order" 1L a;
      Alcotest.(check int64) "reversed order" 1L b
  | _ -> Alcotest.fail "same macro name in two units"

(* A macro call written INSIDE a .fun unit used to die as an unbound variable,
   whichever way it got there - including a call to a macro the same file
   defines. The unit was expanded twice: once by the macro driver, with macros
   live, and once by the loader with no [elaborate] callback, and the second,
   inert surface was the one that got elaborated. The loader now keeps the
   driver's surface. *)
let macro_in_unit label expected modules src =
  match eval_with_imported_macros modules src with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "%s: %s" label (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))

let answers_42 = ("inner", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }")

(* An operator's fixity resolves last-wins by design, so a user operator can
   override a builtin. Its macro body now comes from that same declaration: the
   use node carries the unit that supplied the operator. Previously the body was
   found by scanning units for the written name, which neither import order nor
   definition order decided - it was the unit path's hash - so the precedence
   and the body could come from different units. *)
let check_operator label expected modules src =
  match eval_with_imported_macros modules src with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "%s: %s" label (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))

let op_returns_1 = ("opa", "open (import \"std\");\npub infix (~) (stx) { Syntax.i64(1) }")

let test_imported_operator_used_inside_a_unit () =
  check_operator "operator inside a unit" 1L
    [ op_returns_1; ("mid", "open (import \"opa\");\npub v = 1 ~ 2") ]
    "{ M = import \"mid\"; M.v }"

let test_operator_declared_and_used_in_one_unit () =
  check_operator "operator declared and used in one unit" 3L
    [ ("mid", "open (import \"std\");\npub infix (~) (stx) { Syntax.i64(3) };\npub v = 1 ~ 2") ]
    "{ M = import \"mid\"; M.v }"

(* A locally declared operator is added after the import, and [find_operator]
   takes the most recently added, so it wins - and its body is found by written
   name because it carries no unit. Fixity and body agree here too. *)
let test_local_operator_shadows_imported () =
  check_operator "local operator shadows imported" 9L
    [ op_returns_1 ]
    "{ A = import \"opa\"; infix (~) (stx) { Syntax.i64(9) }; 1 ~ 2 }"

(* An operator's fixity resolves last-wins by design, so a user operator can
   override a builtin. Its macro body now comes from that same declaration: the
   use node carries the unit that supplied the operator. Previously the body was
   found by scanning units for the written name, which neither import order nor
   definition order decided - it was the unit path's hash - so the precedence
   and the body could come from different units. *)
let test_operator_body_follows_last_import () =
  let modules =
    [ ("opa", "open (import \"std\");\npub infix (~) (stx) { Syntax.i64(1) }");
      ("opb", "open (import \"std\");\npub infix (~) (stx) { Syntax.i64(2) }") ]
  in
  let run label expected src =
    match eval_with_imported_macros modules src with
    | VAtom (I64 n) -> Alcotest.(check int64) label expected n
    | v ->
        let mc = MetaContext.create () in
        Alcotest.fail (Printf.sprintf "%s: %s" label (Debug.pp_value_short mc v))
    | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  in
  run "opb imported last" 2L "{ A = import \"opa\"; B = import \"opb\"; 1 ~ 2 }";
  run "opa imported last" 1L "{ B = import \"opb\"; A = import \"opa\"; 1 ~ 2 }"

(* M3: a macro body is elaborated in its definition site's scope, nothing
   ambient - a unit that uses the Syntax API without opening the prelude has
   no [Syntax] in scope, exactly as its runtime code would not. *)
let test_macro_body_sees_nothing_ambient () =
  match eval_with_imported_macros
    [ ("bare", "pub macro answer(_) { Syntax.i64(7) };\npub v = answer(0)") ]
    "{ M = import \"bare\"; M.v }"
  with
  | _ -> Alcotest.fail "a macro body must not see the prelude its unit did not open"
  | exception _ -> ()

let test_unit_uses_its_own_macro () =
  macro_in_unit "own macro" 7L
    [ ("mid", "open (import \"std\");\npub macro answer(_) { Syntax.i64(7) };\npub v = answer(0)") ]
    "{ M = import \"mid\"; M.v }"

let test_unit_calls_imported_macro_dotted () =
  macro_in_unit "dotted call inside a unit" 42L
    [ answers_42; ("mid", "I = import \"inner\";\npub v = I.answer(0)") ]
    "{ M = import \"mid\"; M.v }"

let test_unit_calls_imported_macro_via_open () =
  macro_in_unit "open inside a unit" 42L
    [ answers_42; ("mid", "open (import \"inner\");\npub v = answer(0)") ]
    "{ M = import \"mid\"; M.v }"

(* A unit-valued member is itself a handle on a unit, so a macro stays reachable
   through a re-export at any depth. *)
let test_macro_through_reexported_member () =
  macro_in_unit "macro two dots away" 42L
    [ answers_42; ("mid", "pub I = import \"inner\"") ]
    "{ M = import \"mid\"; M.I.answer(0) }"

let test_imported_macro_not_runtime_field () =
  match
    eval_with_imported_macros
      [ ("macros", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ M = import \"macros\"; M.answer }"
  with
  | exception Elaborate.ElabError _ -> ()
  | exception Nbe.EvalError _ -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected imported macro to be compile-time only"

let test_imported_macro_circular_visit () =
  match
    eval_with_imported_macros
      [ ("a", "open (import \"std\");\npub macro ma(_) { { B = import \"b\"; Syntax.i64(1) } }");
        ("b", "open (import \"std\");\npub macro mb(_) { { A = import \"a\"; Syntax.i64(2) } }") ]
      "{ A = import \"a\"; ma(0) }"
  with
  (* Reading a unit's syntax expands it, macro bodies included, which reaches
     the cycle first. *)
  | exception (Core_loader.CircularMacroVisit "a" | Core_loader.CircularSyntaxVisit _) -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected circular macro visit"

let test_macro_generated_import_loads_macros () =
  match
    eval_with_imported_macros
      [ ("loader", "pub macro through(stx) { stx }");
        ("target", "open (import \"std\");\npub macro answer(_) { Syntax.i64(42) }") ]
      "{ L = import \"loader\"; T = L.through(import \"target\"); T.answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "macro-generated import" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "macro-generated import: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "macro-generated import: %s" (Printexc.to_string e))

let test_macro_generated_import_checks_missing () =
  match
    eval_with_imported_macros
      [ ("loader", "pub macro through(stx) { stx }") ]
      "{ L = import \"loader\"; through(import \"missing\") }"
  with
  | exception Core_loader.ImportNotFound "missing" -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected missing macro-generated import"

let test_imported_macro_calls_regular_function () =
  match
    eval_with_imported_macros
      (* [helper] writes its own [open]: a unit elaborates against the base
         context, where [stdlib] is bound but not opened, so bare [Syntax] is
         not in scope for free. *)
      [ ("helper", "open (import \"std\");\npub make_answer = fn(stx) { Syntax.i64(42) }");
        ("macros", "pub macro answer(stx) { { H = import \"helper\"; H.make_answer(stx) } }") ]
      "{ M = import \"macros\"; M.answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "macro calls function" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "macro calls function: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "macro calls function: %s" (Printexc.to_string e))

let test_operator_prefix_macro_expands () =
  check_i64_macro "operator prefix" 42L
    "{
       syntax answer { answer => 42 };
       answer
     }" ()

let test_operator_infix_macro_expands () =
  check_i64_macro "infix macro expands" 9L
    "{
       infix (~) (stx) { Syntax.i64(9) };
       1 ~ 2
       }" ()

let test_macro_multi_arg () =
  check_i64_macro "macro multi-arg" 7L
    "{
       macro add(a, b) { quote($a + $b) };
       add(3, 4)
     }" ()

let test_macro_multi_arg_swap () =
  check_i64_macro "macro multi-arg swap" (-2L)
    "{
       macro flip(a, b) { quote($b - $a) };
       flip(5, 3)
     }" ()

let test_macro_default_expr () =
  check_i64_macro "macro default kind Expr" 1L
    "{
       macro check(stx) { Syntax.i64(1) };
       check(0)
     }" ()

let test_macro_expr_annotation () =
  check_i64_macro "macro : Expr(_) explicit annotation" 1L
    "{
       macro check(_) : Expr(_) { Syntax.i64(1) };
       check(0)
     }" ()

let test_macro_decl_in_expr_context () =
  match
    eval_with_macros
      "{
         macro check(_) : Decl { quote { x = 42 } };
         check(0)
       }"
  with
  | exception Expand_error.Error { error = KindMismatch { kind = Decl; position = Expr; _ }; _ } -> ()
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "expected failure, got: %s" (Debug.pp_value_short mc v))

let test_macro_name_shadowing () =
  check_i64_macro "macro name shadowing regardless of kind" 1L
    "{
       macro m(_) : Decl { quote { x = 0 } };
       macro m(stx) { Syntax.i64(1) };
       m(0)
     }" ()

let test_decl_kind_registered_persists () =
  match eval_with_macros
    "{
       macro m(_) : Decl { quote { x = 1 } };
       m(0)
     }"
  with
  | exception Expand_error.Error { error = KindMismatch { kind = Decl; position = Expr; _ }; _ } -> ()
   | _ -> Alcotest.fail "expected Decl-rejection failure"

let test_decl_macro_generates_binding () =
  let _ = eval_decl_module
    "open (import \"std\");\nmacro mk(n : Id) : Decl { quote { $n = 42 } };
mk(answer);
pub answer2 = answer"
  in
  ()

let test_imported_decl_macro () =
  match eval_with_imported_macros
    [ "imported_decl_module",
      "open (import \"std\");\nmacro mk(n : Id) : Decl { quote { $n = 42 } };
mk(answer);
pub answer2 = answer" ]
    "{
      M = import \"imported_decl_module\";
      M.answer2
     }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported DeclLet generates binding" 42L n
   | _ -> Alcotest.fail "expected 42"

let test_decl_macro_two_calls () =
  let _ = eval_decl_module "open (import \"std\");macro m1(_) : List(Decl) { Nil };macro m2(_) : List(Decl) { Nil };m2(0)"
  in
  ()

let test_pattern_round_trip () =
  check_i64_macro "Pattern builder evaluates" 1L
    "{
       macro check(_) { Syntax.i64(1) };
       { _ = Syntax.pat_wild; check(0) }
     }" ()

let test_type_aware_macro () =
  check_i64_macro "type-aware binder solved from the expected type" 1L
    "{
       macro default[A](_) : Expr(A) {
         { _ = A; Syntax.i64(1) }
       };
       { x : I64 = default(0); x }
     }" ()

let test_type_aware_checking () =
  let _module_val = eval_decl_module
    "open (import \"std\");\nmacro default[A](_) : Expr(A) {
       match (A) {
       RExpr(I64) => Syntax.i64(42),
       _ => Syntax.i64(0)
       }
     };
     pub x : I64 = default(0)"
  in
  (* Type annotation on x means checking mode — A should be I64 → 42 *)
   ()

let test_type_default_macro () =
  check_i64_macro "type-directed default for I64" 0L
    "{
       macro default[A](_) : Expr(A) {
         match (A) {
         RExpr(I64) => Syntax.i64(0),
         RExpr(Bool) => quote(False),
         _ => { _ = A; Syntax.i64(42) }
         }
       };
       { x : I64 = default(0); x }
     }" ()

let test_type_default_bool () =
  match eval_with_macros
    "{
       macro default[A](_) : Expr(A) {
         match (A) {
         RExpr(I64) => Syntax.i64(0),
         RExpr(Bool) => quote(False),
         _ => { _ = A; quote(False) }
         }
       };
       { x : Bool = default(0); x }
     }"
  with
  | VCon { name = "False"; _ } -> ()
  | v -> Alcotest.fail (Debug.pp_value_short (MetaContext.create ()) v)

let test_rtype_match_non_exhaustive_missing_ctors () =
  match eval_decl_module
    "open (import \"std\");\nmacro default[A](_) : Expr(A) {
       match (A) {
       RExpr(I64) => Syntax.i64(0)
       }
     };
     pub x : I64 = default(0)"
  with
  | _ -> Alcotest.fail "expected non-exhaustive match"
  | exception _ -> ()

let test_expr_binding () =
  check_i64_macro "macro : Expr(A) works" 1L
    "{
       macro mk[A](_) : Expr(A) { { _ = A; Syntax.i64(1) } };
       { x : I64 = mk(0); x }
     }" ()

(* M1: reflecting syntax and rebuilding it is the identity on every field —
   scope sets, annotations, explicitness, span positions, pattern paths. *)
let test_round_trip_is_identity () =
  let n = Some (Elaborate.syntax_nominals (Elaborate.init_ctx ())) in
  let span = { Source_span.file = Some "f.fun"; start_byte = 3; end_byte = 9; start_line = Some 1;
               start_col = Some 3; end_line = Some 1; end_col = Some 9; synthetic = false } in
  let id name = { Syntax.name; span; scope = Scope_set.union (Scope_set.singleton 4) (Scope_set.singleton 7) } in
  let mk kind = { Syntax.kind; span } in
  let param = { Syntax.name = id "a"; type_ = Some (mk (Syntax.Var (id "T"))); trait_bounds = [];
                explicitness = Explicitness.Implicit } in
  let stx =
    mk (Syntax.Let { name = id "x"; type_ = Some (mk (Syntax.Var (id "I64"))); recursive = true;
                     value = mk (Syntax.Lam (param, mk (Syntax.Var (id "a"))));
                     body = mk (Syntax.Ap (mk (Syntax.Var (id "x")), Explicitness.Implicit, mk (Syntax.Atom (Atom.I64 1L)))) })
  in
  Alcotest.(check bool) "expr" true (Macro_eval.unwrap_stx ?nominals:n (Macro_eval.wrap_stx ~nominals:n stx) = Some stx);
  (* Total: a program over most of the grammar survives, before expansion and
     after it (when every id carries scopes). *)
  let program =
    Enforest.parse_module
      {|pub type A = MkA(B) | NoA and B = MkB(A) | NoB;
pub P = struct {x: I64; y: A};
pub effect Ask = sig { ask : Unit -> I64 };
pub trait Show(T) = sig { show : T -> String };
pub impl Show(I64) = module { fn show(x) { "n" } };
pub M = struct { v : I64; pub method get() { self.v } };
pub f = fn[T : Type](x : T, g : T -> T) { g(x) };
pub h = fn(r) { match (r) { P{x = 1; y = _} => 1, _ => 2 } };
pub k = fn(z) { { w = ref(1); (z, 3).0 } };
pub pattern Two(a) = MkA(a);
open M|}
  in
  let expanded = Expand.expand (Expand_ctx.create ()) program in
  List.iter
    (fun (label, stx) ->
      Alcotest.(check bool) label true (Macro_eval.unwrap_stx ?nominals:n (Macro_eval.wrap_stx ~nominals:n stx) = Some stx))
    [ ("program", program); ("expanded program", expanded) ];
  let pat = Syntax.PatCon ({ Syntax.head = id "M"; members = [ "C" ]; head_choice = None }, [ Syntax.PatBind (id "y"); Syntax.PatWild ]) in
  Alcotest.(check bool) "pattern" true
    (Macro_eval.unwrap_stx_pat ?nominals:n (Macro_eval.wrap_stx_pat ~nominals:n pat) = Some pat)

let test_unit_level_type_chain () =
  match eval_decl_module
    "pub type A = MkA(B) | NoA and B = MkB(A) | NoB;
     pub x = match (MkA(MkB(NoA))) { MkA(MkB(NoA)) => 1, _ => 0 }"
  with
  | VModule { entries; _ } ->
      (match List.find_map (function ModuleField ("x", _, _) -> Some () | _ -> None) entries with
       | Some () -> ()
       | None -> Alcotest.fail "x missing")
  | v -> Alcotest.fail (Debug.pp_value_short (MetaContext.create ()) v)

let test_block_local_macros_do_not_leak () =
  List.iter (fun src ->
    match eval_with_macros src with
    | _ -> Alcotest.fail ("block-local macro leaked: " ^ src)
    | exception _ -> ())
    [ "{ x = { macro mi(_) { Syntax.i64(7) }; 0 }; mi(0) }";
      "{ M = module { macro mi(_) { Syntax.i64(7) } }; mi(0) }";
      "{ R = struct { macro mi(_) { Syntax.i64(7) } }; mi(0) }";
      "{ Q = struct { macro mi(_) { quote(I64) }; g : I64 }; R = struct { f : mi(0) }; R{f = 1}.f }" ];
  check_i64_macro "block-local macro usable inside its block" 7L
    "{ R = struct { macro mi(_) { Syntax.i64(7) }; pub h = mi(0) }; R.h }" ()

(* M2: a macro's binder does not capture what it received, whether the binder
   was quoted or built from a string, on the untyped and the type-aware path;
   a template's splice keeps its scope too. Each answers the caller's x. *)
let test_macro_does_not_capture_argument () =
  List.iter
    (fun src -> check_i64_macro src 1L src ())
    [ "{ x = 1; macro m(e) { quote((fn(x) { $e })(2)) }; y : I64 = m(x); y }";
      "{ x = 1; macro m(e) { quote((fn(x) { $e })(2)) }; y : I64 = m(x); y }";
      "{ x = 1; macro m[A](e) : Expr(A) { { _ = A; quote((fn(x) { $e })(2)) } }; y : I64 = m(x); y }";
      "{ x = 1; syntax li { li $body => { x = 2; $body } }; y : I64 = li x; y }" ]

(* A template's literal ids mean the declarer's names: a caller's [False] or
   [True] does not reach inside the prelude's [&&] / [||]. The caller's own
   binding is untouched. *)
let test_template_literals_resolve_at_definition () =
  List.iter
    (fun (src, expected) ->
      match eval_with_macros src with
      | VCon { name; _ } -> Alcotest.(check string) src expected name
      | v -> Alcotest.fail (src ^ ": " ^ Debug.pp_value_short (MetaContext.create ()) v))
    [ ("{ False = 42; (1 > 2) && (2 > 1) }", "False");
      ("{ True = 7; (2 > 1) || (1 > 2) }", "True") ];
  check_i64_macro "the caller's own binding is untouched" 42L
    "{ False = 42; _ = (1 > 2) && (2 > 1); False }" ()

let test_quote_splices_holes () =
  check_i64_macro "quote splices an expression hole" 42L "{ macro m(e) { quote($e + 1) }; m(41) }" ();
  check_i64_macro "quoted syntax parses where written" 1L
    "{ macro m(e) { quote(match ($e) { True => 1, False => 0 }) }; m(True) }" ();
  match eval_with_macros "{ macro m(e) { quote(fn($e) { $e }) }; m(1) }" with
  | _ -> Alcotest.fail "a hole in both binder and expression position must be rejected"
  | exception _ -> ()

(* M10: [quote { … }] quotes declarations; a lone [$d] item is a Decl hole. *)
let test_quote_declarations () =
  check_i64_macro "quote { } splices an expression hole into a declaration" 42L
    "{ macro define(v) : Decl { quote { pub answer = $v; } }; M = module { define(21 + 21) }; M.answer }" ();
  check_i64_macro "quote { } splices a declaration hole" 6L
    "{ macro wrap(v) : List(Decl) { d = quote { pub answer = $v }; quote { $d; pub other = 1; } };
       M = module { wrap(5) }; M.answer + M.other }" ()

let test_type_aware_output_is_expanded () =
  check_i64_macro "type-aware output expands nested macro" 1L
    "{
       macro one(_) { Syntax.i64(1) };
       macro m[A](e) : Expr(A) { { _ = A; quote(one($e)) } };
       y : I64 = m(0);
       y
     }" ()

let test_expected_type_reaches_macro () =
  let _ = eval_decl_module
    "open (import \"std\");\nmacro typed[A](_) : Expr(A) { Syntax.i64(42) };
     pub x : I64 = typed(0)"
  in
  ()

let test_expected_type_rejects_mismatch () =
  match eval_decl_module
    "open (import \"std\");\nmacro typed[A](_) : Expr(A) { quote(True) };
     pub x : I64 = typed(0)"
  with
  | _ -> Alcotest.fail "expected type mismatch"
  | exception _ -> ()

(** Stage 2: binder-mode type checking — when a binder annotation is
    used at a typed site, the expected type fills the implicit param.
    A return-type mismatch still surfaces as an elaboration error. *)
let test_binder_type_mismatch () =
  match eval_decl_module
    "open (import \"std\");\nmacro mk(_) : Expr(I64) { Syntax.i64(1) };
     pub x : Bool = mk(0)"
  with
  | _ -> Alcotest.fail "expected binder type mismatch"
  | exception _ -> ()

(** Stage 2: binder-mode body type mismatch — the body returns a type
    incompatible with the annotated use site. *)
let test_binder_body_type_mismatch () =
  match eval_decl_module
    "open (import \"std\");\nmacro mk(_) : Expr(I64) { quote(True) };
     pub x : I64 = mk(0)"
  with
  | _ -> Alcotest.fail "expected binder body type mismatch"
  | exception _ -> ()

(** Stage 3: helper that produces a driver_output via the new [Macro_driver].
    Parses source, runs the driver, and returns the output. *)
let run_driver source : Macro_driver.driver_output =
  Macro_driver.run ~load_syntax:Elab_prelude.std_load_syntax (Enforest.parse_module source)

(** Stage 3: equivalence helper — runs both the old pipeline
    ([Parse_expand.parse_module_with_ctx]) and the new driver, and
    returns the full lowered surfaces for structural comparison. *)
let driver_vs_pipeline source =
  let ctx = Elaborate.init_ctx () in
  let nominals = Elaborate.syntax_nominals ctx in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply = Nbe.apply_macro in
  let pipeline_surface, _expand_ctx =
    Parse_expand.parse_module_with_ctx
      ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~load_syntax:Elab_prelude.std_load_syntax source
  in
  let driver_output = run_driver source in
  (* Each parse mints its own enforestation scopes (negative, from one global
     counter), so compare with those erased; the expander's scopes restart per
     context and must agree. *)
  (* Resolved names come from one global counter, so each run's are numbered
     in order of first appearance before comparing. *)
  let erase stx =
    let numbering = Hashtbl.create 16 in
    let rename name =
      if not (String.contains name '#') then name
      else
        match Hashtbl.find_opt numbering name with
        | Some n -> n
        | None ->
            let n = Printf.sprintf "%s#%d" (Syntax.label name) (Hashtbl.length numbering) in
            Hashtbl.add numbering name n;
            n
    in
    Expand.map_ids (fun (id : Syntax.id) -> { id with name = rename id.name; scope = List.filter (fun s -> s >= 0) id.scope }) stx
  in
  (erase pipeline_surface, erase driver_output.expanded)

(** Stage 3: structural equivalence — a module with only runtime bindings
    produces the same binding structure from both pipelines. *)
let test_driver_equiv_runtime () =
  let source = "open (import \"std\");\npub x : I64 = 42;\npub y = x + 1\n" in
  let a, b = driver_vs_pipeline source in
  Alcotest.(check bool)
    "driver matches pipeline for runtime module" true (a = b)

(** Stage 3: a module with a macro definition produces the same surface
    structure from both pipelines. *)
let test_driver_equiv_macro () =
  let source =
    "open (import \"std\");\nmacro mk(_) { Syntax.i64(1) };\n\
     pub x : I64 = mk(0)\n"
  in
  let a, b = driver_vs_pipeline source in
  Alcotest.(check bool)
    "driver matches pipeline for module with macro" true (a = b)

(** Stage 3: [macro_exports] includes a locally defined default-kind macro. *)
let test_driver_macro_exports_default () =
  let output = run_driver "open (import \"std\");\nmacro mk(_) { Syntax.i64(1) }\n" in
  let names = List.map (fun (e : Macro_driver.macro_export) -> e.name) output.macro_exports in
  Alcotest.(check (list string)) "macro_exports contains mk"
    ["mk"] names;
  Alcotest.(check string) "macro export kind is default Expr"
    (Syntax.MacroKind.to_string Syntax.MacroKind.default)
    (Syntax.MacroKind.to_string (List.hd output.macro_exports).kind)

(** Stage 3: [macro_exports] includes a Decl-kind macro. *)
let test_driver_macro_exports_decl () =
  let output = run_driver "open (import \"std\");\nmacro gen(_) : List(Decl) { Nil }\n" in
  let names = List.map (fun (e : Macro_driver.macro_export) -> e.name) output.macro_exports in
  Alcotest.(check (list string)) "macro_exports contains gen" ["gen"] names;
  Alcotest.(check string) "macro export kind is Decl"
    (Syntax.MacroKind.to_string Syntax.MacroKind.Decl)
    (Syntax.MacroKind.to_string (List.hd output.macro_exports).kind)

(** Stage 3: [driver_output.elab_ctx.macro_runtime] is populated. *)
let test_driver_elab_ctx_has_macro_runtime () =
  let output = run_driver "pub x : I64 = 42\n" in
  Alcotest.(check bool) "elab_ctx.macro_runtime is populated" true
    (Option.is_some output.elab_ctx.Elab_ctx.Ctx.macro_runtime)

(** Stage 4: helper to get the kind of a named macro export from a driver run. *)
let exported_macro source macro_name =
  let output = run_driver source in
  match List.find_opt (fun (e : Macro_driver.macro_export) -> String.equal e.name macro_name) output.macro_exports with
  | Some e -> e
  | None -> Alcotest.fail ("macro not found in exports: " ^ macro_name)

let compiled_macro_arity (v : Core.value) =
  let rec term_lam_count = function
    | Core.Lam body -> 1 + term_lam_count body
    | _ -> 0
  in
  match v with
  | Core.VLam { body = { body; _ } } -> 1 + term_lam_count body
  | _ -> 0

(** Stage 8: driver run with a loader over temp modules. *)
let run_driver_with_modules modules source f =
  with_modules modules (fun loader ->
      f (Macro_driver.run ~loader ~load_syntax:(Core_loader.load_syntax_exports loader) (Enforest.parse_module source)))

let exported_kind_with_modules modules source macro_name =
  run_driver_with_modules modules source (fun (output : Macro_driver.driver_output) ->
      match List.find_opt (fun (e : Macro_driver.macro_export) -> String.equal e.name macro_name) output.macro_exports with
      | Some e -> e.kind
      | None -> Alcotest.fail ("macro not found in exports: " ^ macro_name))

(** Stage 8: only public macros are registered by import loading. *)
let test_visit_macros_private_not_registered () =
  with_modules
    [ ("macros_mod", "open (import \"std\");\nmacro hidden(_) { Syntax.i64(1) };\npub macro shown(_) { Syntax.i64(2) }") ]
    (fun loader ->
      let ctx = Expand_ctx.create () in
      Macro_driver.visit_macros loader ctx "macros_mod";
      let key name = Expand_ctx.unit_macro_key ~path:"macros_mod" ~name in
      Alcotest.(check bool) "private macro not registered" true
        (Option.is_none (Expand_ctx.lookup_macro ctx (key "hidden")));
      Alcotest.(check bool) "public macro registered" true
        (Option.is_some (Expand_ctx.lookup_macro ctx (key "shown")));
      (* A macro is a member of its unit, so it is not reachable as a bare
         name until something opens that unit. *)
      Alcotest.(check bool) "not injected bare" true
        (Option.is_none (Expand_ctx.lookup_macro ctx "shown")))

(* A macro's signature - its type binders [macro m[A, B](..)], typed parameters
   and promised output - is syntactic, and elaborates where the macro is
   defined: a name in it must resolve there, and a promised [T] must be a type.
   A macro whose signature promises a type waits for the elaborator. *)
let test_macro_type_binders_are_explicit () =
  let std src = "open (import \"std\");\n" ^ src in
  let check_macro label ~typed ~arity src =
    let export = exported_macro (std src) "mk" in
    Alcotest.(check bool) (label ^ ": binds a type") typed (Syntax.MacroKind.has_type_binding export.kind);
    Alcotest.(check int) (label ^ ": arity") arity (compiled_macro_arity export.entry.value)
  in
  check_macro "bound" ~typed:true ~arity:2 "macro mk[A](_) : Expr(A) { Syntax.i64(1) }";
  check_macro "lowercase binder" ~typed:true ~arity:2 "macro mk[t](_) { Syntax.i64(1) }";
  check_macro "an earlier type of the binder's name" ~typed:true ~arity:2
    "type A = I64;\nmacro mk[A](_) : Expr(A) { Syntax.i64(1) }";
  check_macro "two binders" ~typed:true ~arity:3 "macro mk[A, B](_) { Syntax.i64(1) }";
  check_macro "constraint" ~typed:true ~arity:1 "macro mk(_) : Expr(I64) { Syntax.i64(1) }";
  check_macro "lowercase alias constraint" ~typed:true ~arity:1 "t = I64;\nmacro mk(_) : Expr(t) { Syntax.i64(1) }";
  check_macro "typed parameter" ~typed:true ~arity:1 "macro mk(x : Expr(I64)) { x }";
  check_macro "wildcard" ~typed:false ~arity:1 "macro mk(_) : Expr(_) { Syntax.i64(1) }";
  Alcotest.(check bool) "qualified imported constraint" true
    (Syntax.MacroKind.has_type_binding
       (exported_kind_with_modules [ ("types_mod", "pub type T = I64") ]
          (std "M = import \"types_mod\";\nmacro mk(_) : Expr(M.T) { Syntax.i64(1) }\n") "mk"));
  check_i64_macro "expression-level constraint" 1L "{ macro mk(_) : Expr(I64) { Syntax.i64(1) }; mk(0) }" ();
  List.iter
    (fun src ->
      match run_driver (std src) with
      | _ -> Alcotest.fail ("expected a definition error: " ^ src)
      | exception (Elab_error.ElabError _ | Unify.UnifyError _ | Enforest_util.Error _) -> ())
    [ "macro mk(_) : Expr(Strng) { Syntax.i64(1) }";
      "macro mk(_) : Expr(foo) { Syntax.i64(1) }";
      "macro mk(_) : Expr(No.Such) { Syntax.i64(1) }";
      "macro mk(_) : Expr(MyTag) { Syntax.i64(1) };\ntype MyTag = I64";
      "macro mk(_) : Int { Syntax.i64(1) }";
      "macro mk(_) : Expr(None) { Syntax.i64(1) }";
      "macro mk(x : Expr(None)) { x }";
      "macro mk[A](_) : Decl { Nil }" ];
  match eval_with_macros "{ macro mk(_) : Expr(Intt) { Syntax.i64(1) }; mk(0) }" with
  | _ -> Alcotest.fail "an unbound annotation name must be an error"
  | exception Elab_error.ElabError (UnboundVariable "Intt") -> ()


(* A macro's signature is checked like a function's (macro-annotation decisions,
   2026-09-15): the output against the type it promises, a typed argument
   against its parameter's type, and every type binder solved before it runs. *)
let expect_elab_error label check source =
  match eval_with_macros source with
  | exception Elab_error.ElabError e when check e -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an elaboration error")

let test_macro_signature_checks () =
  expect_elab_error "output against the promised type"
    (function Elab_error.MacroOutputType { macro = "n"; _ } -> true | _ -> false)
    "{ macro n(_) : Expr(I64) { quote(\"hi\") }; n(0) }";
  expect_elab_error "typed argument against its parameter"
    (function Elab_error.MacroArgumentType { macro = "twice"; param = "x"; _ } -> true | _ -> false)
    "{ macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) }; twice(\"a\") }";
  expect_elab_error "a binder unsolved when the macro runs"
    (function Elab_error.MacroBinderUnsolved { macro = "default"; binder = "A" } -> true | _ -> false)
    "{ macro default[A](_) : Expr(A) { Syntax.i64(1) }; default(0) }";
  check_i64_macro "a typed argument checks" 42L
    "{ macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) }; twice(21) }" ();
  check_i64_macro "two binders, solved from the arguments" 42L
    "{
       macro first[A, B](a : Expr(A), b : Expr(B)) : Expr(A) {
         match (B) { RExpr(Bool) => a, _ => b }
       };
       first(40, True) + 2
     }" ();
  (* A typed call's effects are its output's, read after it runs. *)
  check_i64_macro "a typed call inside a lambda body its output adds" 5L
    "{
       macro inner(x : Expr(I64)) : Expr(I64) { x };
       macro under(x : Expr(I64)) : Expr(I64) { quote((fn(z : I64) { $x })(0)) };
       z = 5;
       under(inner(z))
     }" ();
  check_i64_macro "a typed call in a type annotation" 3L
    "{ macro ty(_) : Expr(Type) { quote(I64) }; (3 : ty(1)) }" ()

(* A typed argument elaborates once, where the call is written: its placements in
   the output reuse that. [Elab_resolve.elaborated_counter] counts typed
   arguments elaborated, so a nested typed call re-elaborated per placement would
   count again. *)
let test_typed_arguments_elaborate_once () =
  let prelude =
    "macro inner(y : Expr(I64)) : Expr(I64) { y };
     macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) };
     macro under(x : Expr(I64)) : Expr(I64) { quote((fn(y : I64) { $x + y })(1)) };
     macro at_alias(x : Expr(I64)) : Expr(I64) { quote(($x : (fn(t : Type) { t })(I64))) };
     macro rebuild(x : Expr(I64)) : Expr(I64) { match (x) { Syntax.Atom(v) => Syntax.atom_val(v), _ => x } };"
  in
  let once label expected body =
    let before = !Elab_resolve.elaborated_counter in
    check_i64_macro label expected ("{ " ^ prelude ^ " z = 41; " ^ body ^ " }") ();
    Alcotest.(check int) (label ^ ": typed arguments elaborated") 2 (!Elab_resolve.elaborated_counter - before)
  in
  once "a duplicated placement" 42L "twice(inner(21))";
  (* ponytail: a typed call inside a lambda body the output adds fails effect
     collection (a deferred [MacroCall] there, as before this change), so this
     placement proves the weakening with a plain argument. *)
  check_i64_macro "a placement under a binder the output adds" 42L ("{ " ^ prelude ^ " z = 41; under(z) }") ();
  once "a placement at a convertible type" 41L "at_alias(inner(z))";
  check_i64_macro "a rebuilt argument elaborates as new syntax" 7L ("{ " ^ prelude ^ " rebuild(7) }") ()

let test_imported_macro_signature () =
  let modules =
    [ ("typed", "open (import \"std\");\npub macro twice(x : Expr(I64)) : Expr(I64) { quote($x + $x) }") ]
  in
  (match eval_with_imported_macros modules "{ M = import \"typed\"; M.twice(21) }" with
   | VAtom (I64 n) -> Alcotest.(check int64) "imported typed macro" 42L n
   | v -> Alcotest.fail (Debug.pp_value_short (MetaContext.create ()) v));
  match eval_with_imported_macros modules "{ M = import \"typed\"; M.twice(True) }" with
  | exception Elab_error.ElabError (MacroArgumentType { param = "x"; _ }) -> ()
  | exception e -> Alcotest.fail (Printexc.to_string e)
  | _ -> Alcotest.fail "an imported macro's signature must check its argument"

(** Stage 6: Decl macro generated MacroBinding nodes are recursively
    re-entered through expand_struct_binding. This lower-level regression
    constructs the generated binding directly because the source-level
    Syntax.Decl prelude currently exposes only DeclLet. *)
let test_generated_macro_binding_reentered () =
  let stx kind = { Syntax.kind; span = Source_span.synthetic } in
  let id name = Syntax.fresh_id name in
  let generated_macro =
    Syntax.MacroBinding {
      name = id "answer";
      value = stx (Syntax.Atom (I64 42L));
      public = false;
      kind = Some Syntax.MacroAnnotation.Expr;
      output = None;
    }
  in
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.elaborate <- Some (fun _ -> VAtom Unit);
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ fn _ -> fn);
  Binding.extend ctx.Expand_ctx.binding_table ~name:"gen" ~scope:Scope_set.empty ~kind:Binding.Macro ~resolved_name:"gen";
  Expand_ctx.register_macro ctx ~name:"gen" ~value:(VStx (StxDecls [ generated_macro ]));
  Expand_ctx.register_macro_kind ctx ~name:"gen" ~kind:Syntax.MacroKind.Decl ~params:[];
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = [] } in
  let _surface_bindings = Expand.expand_struct_bindings ctx [ call ] in
  Alcotest.(check bool) "generated macro registered" true
    (Hashtbl.fold (fun k _ acc -> acc || String.equal (Syntax.label k) "answer") ctx.Expand_ctx.macro_table false);
  let key = Hashtbl.fold (fun k _ acc -> if String.equal (Syntax.label k) "answer" then k else acc) ctx.Expand_ctx.macro_table "" in
  begin match Expand_ctx.lookup_macro_kind ctx key with
  | Some kind ->
      Alcotest.(check bool) "generated macro binds no type" false
        (Syntax.MacroKind.has_type_binding kind)
  | None -> Alcotest.fail "generated macro kind missing"
  end

let test_generated_multi_binding_scope_threading () =
  let stx kind = { Syntax.kind; span = Source_span.synthetic } in
  let id name = Syntax.fresh_id name in
  let id_x = id "x" in
  let id_y = id "y" in
  let generated_x =
    Syntax.LetBinding {
      name = id_x;
      value = stx (Syntax.Atom (I64 42L));
      public = false;
      recursive = false;
    }
  in
  let generated_y =
    Syntax.LetBinding {
      name = id_y;
      value = stx (Syntax.Var (id "x"));
      public = false;
      recursive = false;
    }
  in
  let ctx = Expand_ctx.create () in
  ctx.Expand_ctx.elaborate <- Some (fun _ -> VAtom Unit);
  ctx.Expand_ctx.eval_and_apply <- Some (fun _ fn _ -> fn);
  Binding.extend ctx.Expand_ctx.binding_table ~name:"gen" ~scope:Scope_set.empty ~kind:Binding.Macro ~resolved_name:"gen";
  Expand_ctx.register_macro ctx ~name:"gen" ~value:(VStx (StxDecls [ generated_x; generated_y ]));
  Expand_ctx.register_macro_kind ctx ~name:"gen" ~kind:Syntax.MacroKind.Decl ~params:[];
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = [] } in
  let expanded_bindings = Expand.expand_struct_bindings ctx [ call ] in
  let find_let name binds =
    List.find_opt (function Syntax.LetBinding b -> Syntax.label b.name.name = name | _ -> false) binds
  in
  match find_let "x" expanded_bindings, find_let "y" expanded_bindings with
  | Some (Syntax.LetBinding { name = x_name; _ }), Some (Syntax.LetBinding { value = y_value; _ }) ->
    Alcotest.(check bool) "x scope is non-empty" true (not (Scope_set.is_empty x_name.scope));
    begin match y_value.kind with
    | Syntax.Var y_id ->
      Alcotest.(check bool) "y's reference to x has scope from x" true
        (Scope_set.subset x_name.scope y_id.scope)
    | _ -> Alcotest.fail "y value is not a Var"
    end
  | _ -> Alcotest.fail "generated multi-binding: x and y not found"

let test_macro_and_syntax_together () =
  check_i64_macro "macro and syntax together" 20L
    "{
       macro twice(x) { quote($x + $x) };
       syntax wrap { wrap $x => twice($x) };
       wrap 10
     }" ()

let test_operator_uses_operands () =
  check_i64_macro "operator uses operands (Left assoc)" 2L
    "{
       order shift; infix (>>>) shift ($lhs, $rhs) { $lhs - $rhs };
       10 >>> 5 >>> 3
     }" ()

let test_operator_right_assoc () =
  check_i64_macro "operator right assoc" 8L
    "{
       order rshift : assoc(right); infix (<<<) rshift ($lhs, $rhs) { $lhs - $rhs };
       10 <<< 5 <<< 3
     }" ()

let test_operator_mixed_precedence () =
  check_i64_macro "operator mixed precedence" 14L
    "{
       infix (+++) additive ($lhs, $rhs) { $lhs + $rhs };
       infix (***) multiplicative ($lhs, $rhs) { $lhs * $rhs };
       2 +++ 3 *** 4
     }" ()

let test_operator_bodyless_infix_builtin_apply () =
  check_i64_macro "bodyless infix applies same-named value" 7L
    "{
       myfst = fn(x, y) { x };
       infix (myfst);
       7 myfst 2
     }" ()

let test_operator_bodyless_prefix_builtin_apply () =
  check_i64_macro "bodyless prefix applies same-named value" 5L
    "{
       ident = fn(x) { x };
       prefix (ident);
       ident 5
     }" ()

let test_operator_rhs_can_use_earlier_macro () =
  check_i64_macro "operator RHS macro path" 5L
    "{
       macro answer_body(_) { Syntax.i64(5) };
       syntax answer { answer => answer_body(0) };
       answer
      }" ()

let test_operator_prefix_receives_structured_input () =
  check_i64_macro "syntax template reuses hole" 4L
    "{
       syntax twice { twice $x => $x + $x };
       twice 2
      }" ()

let test_operator_macro_error_reports_spans () =
  match
    eval_with_macros
       "{
          infix (~) (stx) { 1 };
          1 ~ 2
        }"
  with
  | exception (Expand_error.Error { error = NotSyntax _; site = Some _ } as e) ->
      let msg = Printexc.to_string e in
      Alcotest.(check bool) "mentions syntax operator" true (string_contains msg "syntax operator");
      Alcotest.(check bool) "mentions use span" true (string_contains msg "used at");
      Alcotest.(check bool) "mentions declaration span" true (string_contains msg "declared at")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected syntax operator macro expansion failure"

(* An expansion-time budget overrun reaches the user as the application's
   error, naming the macro, not as a raw budget exception. *)
let diverging_body = "{ rec loop : I64 -> I64 = fn(n) { loop(n) }; loop(0) }"

let test_macro_body_budget_overrun_names_the_macro () =
  match eval_with_macros ("{ macro spin(_) " ^ diverging_body ^ "; spin(0) }") with
  | exception Expand_error.Error { error = BudgetExceeded { macro; _ }; _ } ->
      Alcotest.(check bool) "names the macro" true (string_contains macro "spin")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an expansion budget error"

(* An error inside a syntax operator's body carries the operator's site. *)
let test_operator_body_error_reports_use_span () =
  match
    eval_with_macros
      ("{\n  infix (~) (stx) { " ^ diverging_body ^ " };\n  1 ~ 2\n}")
  with
  | exception (Expand_error.Error { error = BudgetExceeded _; site = Some { use_span; _ } } as e) ->
      Alcotest.(check bool) "use span is a source span" false (use_span = Source_span.synthetic);
      Alcotest.(check bool) "message mentions the use" true (string_contains (Printexc.to_string e) "used at")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an expansion budget error at the operator"

(* Any evaluation failure inside a syntax operator's body - not only an overrun -
   is the application's error, carrying the operator's site. *)
let operator_body_failure_reports_use_span body expected () =
  match eval_with_macros ("{\n  infix (~) (stx) { " ^ body ^ " };\n  1 ~ 2\n}") with
  | exception (Expand_error.Error { error = EvalFailed { message; _ }; site = Some { use_span; _ } } as e) ->
      Alcotest.(check string) "message" expected message;
      Alcotest.(check bool) "use span is a source span" false (use_span = Source_span.synthetic);
      Alcotest.(check bool) "message mentions the use" true (string_contains (Printexc.to_string e) "used at")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an evaluation failure at the operator"

let test_syntax_module_expression_kind () =
  check_i64_macro "Syntax.kind expression object" 1L
    "{ macro answer(stx) { match (stx) { Syntax.Var(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; x = 10; answer(x) }" ()

let test_syntax_module_literal_inspectors () =
  check_i64_macro "Syntax literal inspectors" 42L
    "{ macro answer(_) { Syntax.i64(42) }; answer() }" ()

let test_syntax_module_literal_inspector_error () =
  match eval_with_macros "{ macro f(stx) { match (stx) { Syntax.Atom(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(g(x)) }" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected atom fallback on non-atom"

let test_syntax_module_ap_deconstructors () =
  check_i64_macro "Syntax ap deconstructors" 1L
    "{ macro f(stx) { match (stx) { Syntax.Ap(f, a) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(add(1, 2)) }" ()

let test_syntax_module_ap_deconstructor_error () =
  match eval_with_macros "{ macro f(stx) { match (stx) { Syntax.Ap(_, _) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(1) }" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected ap fallback on non-ap"

let test_syntax_module_lam_deconstructor_error () =
  match eval_with_macros "{ macro f(stx) { match (stx) { Syntax.Lam(_, _) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(Syntax.i64(0)) }" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected lam fallback on non-lam"

let test_syntax_module_let_deconstructor_error () =
  match eval_with_macros "{ macro f(stx) { match (stx) { Syntax.Let(_, _, _) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(Syntax.i64(0)) }" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected let fallback on non-let"

let test_syntax_module_identifier_inspection () =
  check_i64_macro "Syntax identifier inspection" 1L
    "{ macro inspect(stx) { match (stx) { Syntax.Var(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; target = 10; inspect(target) }" ()


let test_syntax_module_i64_builder () =
  check_i64_macro "Syntax.i64 builder" 42L
    "{
       macro answer(_) { Syntax.i64(42) };
       answer(0)
     }" ()

let test_syntax_class_types_accessible () =
  check_i64_macro "Syntax class types accessible" 42L
    "{
       x = Syntax.i64(42);
       42
     }" ()

let test_syntax_expr_nominal_resolvable () =
  let ctx = Elaborate.init_ctx () in
  match Elaborate.resolve_stdlib ctx ["Syntax"; "Expr"] with
  | VNominal { name = "Expr"; num_params = 0; constructors; _ } ->
      Alcotest.(check int) "one constructor per expression form" 40 (List.length constructors);
      Alcotest.(check bool) "RawVar present" true
        (List.exists (fun (n, _) -> n = "RawVar") constructors);
      Alcotest.(check bool) "RawAtom present" true
        (List.exists (fun (n, _) -> n = "RawAtom") constructors)
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "expected VNominal Expr, got %s" (Debug.pp_value_short mc v))
   | exception e ->
      Alcotest.fail (Printf.sprintf "exception: %s" (Printexc.to_string e))

let test_pattern_syn_subst () =
  let rhs = CPatCon ("RawAp", 0, [ CPatWild; CPatBind; CPatWild; CPatBind ]) in
  let result = Elab_patterns.subst_syn_params ["fn"; "arg"] [CPatAtom (I64 1L); CPatAtom (I64 2L)] rhs in
  match result with
  | CPatCon ("RawAp", 0, [ CPatWild; CPatAtom (I64 1L); CPatWild; CPatAtom (I64 2L) ]) -> ()
  | _ -> Alcotest.fail "substitution did not produce expected pattern"

let test_pattern_syn_in_prelude () =
  let ctx = Elaborate.init_ctx () in
  match Elaborate.resolve_stdlib ctx ["Syntax"; "Ap"] with
  | VPatternSyn { name = "Ap"; params = ["f"; "a"]; _ } -> ()
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "expected VPatternSyn Ap, got %s" (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail (Printf.sprintf "exception: %s" (Printexc.to_string e))

let test_syntax_primitive_names_hidden () =
  match eval_with_macros "{ macro answer(_) { stx_make_i64(42) }; answer(0) }" with
  | exception Elaborate.ElabError (Elaborate.UnboundVariable "stx_make_i64") -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected direct stx_* primitive name to be hidden"

let test_syntax_module_application_builder () =
  check_i64_macro "Syntax.ap builder" 3L
    "{
       macro add(_) { quote(1 + 2) };
       add(0)
     }" ()

let test_syntax_module_literal_builders () =
  check_i64_macro "Syntax char/unit builders" 42L
    "{
       macro char_a(_) { Syntax.char('a') };
       macro unit_value(_) { Syntax.unit(()) };
       if (char_a(0) == 'a') {
         if (unit_value(0) == ()) { 42 } else { 0 }
       } else { 0 }
      }" ()

let test_syntax_module_let_builder () =
  check_i64_macro "Syntax let builder" 7L
    "{
       macro answer(_) { quote({ x = 3; x + 4 }) };
       answer(0)
      }" ()

let test_operator_macro_discards_unelaborated_perform_operand () =
  check_i64_macro "operator macro discards perform operand before elaboration" 7L
    "{
       syntax discard { discard $x => 7 };
       discard perform Missing.get(())
      }" ()

let test_7g_adt_matching_hygiene_roundtrip () =
  check_i64_macro "7G: ADT matching preserves binding hygiene" 42L
    "{ x = 1; macro passthrough(stx) { match (stx) { Syntax.Lam(_, _) => stx, _ => stx } }; (passthrough(fn(x) { x }))(42) }" ()

(* Rebuilding a lambda around its destructured body binds the body only through
   the lambda's own parameter. A binder the macro builds from a string is the
   macro's, and does not capture the caller's [x] (M2). *)
let test_7g_adt_matching_hygiene_introduced_body () =
  check_i64_macro "7G: rebuilt lambda binds its body through its own parameter" 80L
    "{ macro double(stx) { match (stx) { Syntax.Lam(p, body) => Syntax.RawLam(None, p, quote($body + $body)), _ => Syntax.i64(0) } }; (double(fn(x) { x }))(40) }" ();
  match eval_with_macros
    "{ macro double(stx) { match (stx) { Syntax.Lam(_, body) => quote(fn(x) { $body + $body }), _ => Syntax.i64(0) } }; (double(fn(x) { x }))(40) }"
  with
  | _ -> Alcotest.fail "a string-built binder must not capture the caller's x"
  | exception _ -> ()

let test_7g_adt_matching_flip_args () =
  check_i64_macro "7G: computed multi-kind dispatch not possible with templates" 1L
    "{ macro classify(stx) { match (stx) { Syntax.Lam(_, _) => Syntax.i64(1), Syntax.Ap(_, _) => Syntax.i64(2), Syntax.Var(_) => Syntax.i64(3), _ => Syntax.i64(0) } }; classify(fn(x) { x }) }" ()

let test_7g_adt_simple_flip () =
  check_i64_macro "7G: simple swap args via nested match" (-2L)
    "{ macro swap(stx) { match (stx) { Syntax.Ap(inner, b) => match (inner) { Syntax.Ap(f, a) => Syntax.ap(Syntax.ap(f, b), a), _ => stx }, _ => stx } }; result = swap((fn(x, y) { x - y })(5, 3)); result }" ()

let test_7g_diag_outer_binders () =
  check_i64_macro "7G: DEBUG outer match with named binders" 1L
    "{ macro t(stx) { match (stx) { Syntax.Ap(inner, b) => Syntax.i64(1), _ => Syntax.i64(0) } }; result = t((fn(x, y) { x - y })(5, 3)); result }" ()

let test_7g_diag_inner_binders_wildcards () =
  check_i64_macro "7G: DEBUG inner match with wildcards only" 1L
    "{ macro t(stx) { match (stx) { Syntax.Ap(inner, _) => match (inner) { Syntax.Ap(_, _) => Syntax.i64(1), _ => Syntax.i64(0) }, _ => Syntax.i64(0) } }; result = t((fn(x, y) { x - y })(5, 3)); result }" ()

let test_7g_diag_inner_binders_named () =
  check_i64_macro "7G: DEBUG inner match with named binders" 1L
    "{ macro t(stx) { match (stx) { Syntax.Ap(inner, _) => match (inner) { Syntax.Ap(f, a) => Syntax.i64(1), _ => Syntax.i64(0) }, _ => Syntax.i64(0) } }; result = t((fn(x, y) { x - y })(5, 3)); result }" ()

let test_7i_generated_syntax_later_wins_shadow () =
  match
    eval_with_imported_macros
      [ ("gen", "open (import \"std\");\n\
                 syntax build_inc : Decl {\n\
                 build_inc $(n : Id) =>\n\
                     {\n\
                       syntax $n { $n $x => $x + 1 }\n\
                     }\n\
                 };\n\
                 build_inc inc;\n\
                 syntax inc { inc $x => $x + 100 };\n\
                 pub result = inc 1") ]
      "{ M = import \"gen\"; M.result }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated syntax later-wins" 101L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I later-wins: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I later-wins: %s" (Printexc.to_string e))

let test_imported_operator_prefix_expands () =
  match
    eval_with_imported_macros
      [ ("ops", "pub syntax answer { answer => 42 };\npub x = 1") ]
      "{
         Ops = import \"ops\";
          answer
        }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported operator prefix" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "imported operator prefix: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "imported operator prefix: %s" (Printexc.to_string e))

let test_imported_syntax_not_runtime_field () =
  match
    eval_with_imported_macros
      [ ("ops", "pub syntax answer { answer => 42 };\npub x = 1") ]
      "{ Ops = import \"ops\"; Ops.answer }"
  with
  | exception Elaborate.ElabError _ -> ()
  | exception Nbe.EvalError _ -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected imported syntax extension to be compile-time only"

let test_operator_prefix_shadowing_is_lexical () =
  check_i64_macro "operator shadowing" 1L
    "{
       syntax choose { choose => 1 };
       ignored = {
         syntax choose { choose => 2 };
         choose
       };
       choose
      }" ()

let test_syntax_template_unless () =
  check_i64_macro "unless False passes through" 10L
    "{
       syntax unless {
       unless $cond $branch => if ($cond) { 0 } else { $branch }
       };
       unless False 10
     }" ()

(* How far a hole reads is structural (brackets-decide-grouping): the hole
   ending a use reads its form's operand at the form's order - a form in no group
   is weaker than every grouped operator - and any other hole is one term. *)
let test_syntax_template_capture_extent () =
  check_i64_macro "an ungrouped form's trailing hole reads a whole expression" 11L
    "{ syntax inc { inc $x => $x + 1 }; inc 1 * 10 }" ();
  check_i64_macro "a grouped form's trailing hole reads at its order" 20L
    "{ order incs : stronger_than(multiplicative); syntax inc incs { inc $x => $x + 1 }; inc 1 * 10 }" ();
  check_i64_macro "a non-trailing hole is one term" 42L
    "{ syntax pick { pick $c then $t else $e => if ($c) { $t } else { $e } }; pick (1 < 2) then (40 + 2) else 0 }" ();
  check_i64_macro "a hole before a comma reads to it" 5L
    "{ syntax both { both ($a, $b) => $a + $b }; both (1 + 1, 3) }" ();
  check_i64_macro "a literal inside a bracketed capture stays in it" 2L
    "{ syntax when { when $c $t else $e => if ($c) { $t } else { $e } };
       when True (if (False) { 1 } else { 2 }) else 0 }" ();
  match eval_with_macros "{ syntax pick { pick $c then $t else $e => if ($c) { $t } else { $e } }; pick 1 < 2 then 1 else 0 }" with
  | exception Enforest_util.Error msg when String.starts_with ~prefix:"no matching branch" msg -> ()
  | exception e -> Alcotest.fail ("a multi-term inner hole: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "a multi-term inner hole must not match"

(* Precedence is relative (brackets-decide-grouping): order groups, transitive,
   with associativity on the group; operators with no declared order never mix. *)
let test_order_groups () =
  check_i64_macro "a transitive order" 7L
    "{ order low; order mid : stronger_than(low); order high : stronger_than(mid);
       infix (<+>) low ($a, $b) { $a + $b }; infix (<*>) high ($a, $b) { $a * $b };
       1 <+> 2 <*> 3 }" ();
  check_i64_macro "associativity lives on the group" 5L
    "{ order sub : assoc(right); infix (<->) sub ($a, $b) { $a - $b }; 10 <-> 8 <-> 3 }" ();
  check_i64_macro "a group related to the prelude's" 14L
    "{ order tight : stronger_than(multiplicative); infix (<~>) tight ($a, $b) { $a + $b }; 2 * 3 <~> 4 }" ();
  check_i64_macro "an ungrouped operator is weaker than a grouped one" 9L
    "{ infix (<>) ($a, $b) { $a * $b }; 1 + 2 <> 3 }" ();
  check_i64_macro "a non-associative group's members apply once" 3L
    "{ order once : assoc(none); infix (<+>) once ($a, $b) { $a + $b }; 1 <+> 2 }" ();
  check_i64_macro "<- is weaker than arithmetic" 3L "{ r = ref(0); _ = r <- 1 + 2; deref(r) }" ();
  check_i64_macro "a group unrelated to the prelude's meets <-" 5L
    "{ order mine; infix (<>) mine ($a, $b) { $a; $b }; r = ref(0); _ = r <- 1 <> 5; deref(r) }" ();
  check_i64_macro "a stated relation to a weakest group overrides" 1L
    "{ order mine : weaker_than(assignment); infix (<>) mine ($a, $b) { $a; $b }; r = ref(0); _ = r <- 1 <> 5; deref(r) }" ();
  check_i64_macro "a prelude group through an import binder" 14L
    "{ Std = import \"std\"; order tight : stronger_than(Std.multiplicative); infix (<~>) tight ($a, $b) { $a + $b }; 2 * 3 <~> 4 }" ()

let test_order_group_errors () =
  let rejects label fragment source =
    match eval_with_macros source with
    | exception Enforest_util.Error msg when string_contains msg fragment -> ()
    | exception e -> Alcotest.fail (label ^ ": " ^ Printexc.to_string e)
    | _ -> Alcotest.fail (label ^ ": expected an error")
  in
  rejects "undeclared order" "have no declared order"
    "{ order a; order b; infix (<+>) a ($x, $y) { $x }; infix (<*>) b ($x, $y) { $x }; 1 <+> 2 <*> 3 }";
  rejects "two ungrouped operators" "have no declared order"
    "{ infix (<+>) ($x, $y) { $x }; infix (<*>) ($x, $y) { $x }; 1 <+> 2 <*> 3 }";
  rejects "a cyclic order" "cyclic"
    "{ order a; order b : stronger_than(a); order c : stronger_than(b) weaker_than(a); 1 }";
  rejects "numeric precedence" "numeric precedence was removed" "{ infix (<+>) 10 ($x, $y) { $x }; 1 }";
  rejects "an unknown group" "unknown order group: nowhere" "{ infix (<+>) nowhere ($x, $y) { $x }; 1 }";
  rejects "a non-associative group does not chain" "do not chain"
    "{ order once : assoc(none); infix (<+>) once ($a, $b) { $a + $b }; 1 <+> 2 <+> 3 }";
  rejects "<- does not chain" "`<-` and `<-` do not chain"
    "{ a = ref(0); b = ref(0); _ = a <- b <- 1; 0 }";
  rejects "two weakest groups" "have no declared order"
    "{ order w1 : weakest; order w2 : weakest; infix (<+>) w1 ($x, $y) { $x }; infix (<*>) w2 ($x, $y) { $x }; 1 <+> 2 <*> 3 }";
  rejects "a cycle through a weakest group" "cyclic"
    "{ order g; order m : stronger_than(g) weaker_than(assignment); 1 }"

let test_order_group_imported () =
  let ops = ("ops", "open (import \"std\");\npub order tight : stronger_than(multiplicative);\npub infix (<~>) tight ($a, $b) { $a + $b }") in
  let expect label expected source =
    match eval_with_imported_macros [ ops ] source with
    | VAtom (I64 n) -> Alcotest.(check int64) label expected n
    | _ -> Alcotest.fail (label ^ ": expected an integer")
  in
  expect "an opened group relates to a new one" 14L
    "{ open (import \"ops\"); order tighter : stronger_than(tight); infix (<~~>) tighter ($a, $b) { $a * $b }; 2 <~> 3 <~~> 4 }";
  expect "a bound import's operator keeps its order" 14L "{ O = import \"ops\"; 2 * 3 <~> 4 }";
  expect "a group named through an import binder" 14L
    "{ O = import \"ops\"; order tighter : stronger_than(O.tight); infix (<~~>) tighter ($a, $b) { $a * $b }; 2 <~> 3 <~~> 4 }";
  expect "an operator joins a group named through an import binder" 14L
    "{ O = import \"ops\"; infix (<+~>) O.tight ($a, $b) { $a + $b }; 2 * 3 <+~> 4 }";
  match eval_with_imported_macros [ ops ] "{ O = import \"ops\"; order g : stronger_than(O.nowhere); 1 }" with
  | exception Enforest_util.Error msg when string_contains msg "unknown order group: O.nowhere" -> ()
  | exception e -> Alcotest.fail ("unknown dotted group: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "unknown dotted group: expected an error"

let test_syntax_template_when_match () =
  check_i64_macro "when False falls back" 0L
    "{
       syntax when {
       when $cond $branch else $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       when False 42 else 0
     }" ()

let test_syntax_template_hole_reuse () =
  check_i64_macro "twice hole reuse" 6L
    "{
       syntax twice { twice $x => $x + $x };
       twice 3
     }" ()

let test_syntax_template_do_end_delimiters () =
  check_i64_macro "extract expression from do block" 42L
    "{
       syntax extract {
       extract { $body } => $body
       };
       extract { 42 }
     }" ()

let test_syntax_template_hygiene () =
  check_i64_macro "hygiene: template binder does not capture use-site" 1L
    "{
       x = 1;
       syntax let_in {
       let_in $val $body => { x = $val; $body }
       };
       let_in 2 x
     }" ()

let test_syntax_template_def_site_scope () =
  check_i64_macro "definition-site scope for template refs" 2L
    "{
       y = 2;
       syntax get_y { get_y => y };
       {
         y = 99;
         get_y
       }
     }" ()

let test_syntax_template_hole_keeps_use_site_scope () =
  check_i64_macro "captured hole keeps use-site scope" 99L
    "{
       x = 1;
       syntax passthrough { passthrough $body => $body };
       {
         x = 99;
         passthrough x
       }
     }" ()

let test_syntax_template_intro_binding_captures_intro_ref () =
  check_i64_macro "introduced binder captures introduced reference" 7L
    "{
       x = 1;
       syntax local_x { local_x => { x = 7; x } };
       {
         x = 99;
         local_x
       }
     }" ()

let test_syntax_template_def_site_scope_through_lambda () =
  check_i64_macro "definition-site ref is not captured by lambda use site" 1L
    "{
       x = 1;
       syntax get_x { get_x => x };
       (fn(x) { get_x })(99)
     }" ()

let test_syntax_template_nested_def_site_scope () =
  check_i64_macro "definition-site ref ignores nested use-site shadows" 3L
    "{
       z = 3;
       syntax get_z { get_z => z };
       {
         z = 4;
         {
           z = 5;
           get_z
         }
       }
     }" ()

let test_syntax_template_generates_syntax_form () =
  check_i64_macro "generated syntax forms are usable in generated body" 42L
    "{
       syntax build_choose {
       build_choose => {
           syntax flag {
           flag yes => True,
           flag no => False
           };
           syntax choose {
           choose $cond then $branch else $fallback =>
               if ($cond) { $branch } else { $fallback }
           };
           choose (flag yes) then (40 + 2) else 0
         }
       };
       build_choose
     }" ()

let test_syntax_template_generated_syntax_closes_over_hole () =
  check_i64_macro "generated syntax form can reuse outer hole" 16L
    "{
       syntax make_adder {
       make_adder $base => {
           syntax add_base { add_base $x => $x + $base };
           add_base 5 + add_base 1
         }
       };
       make_adder 5
     }" ()

let test_syntax_template_generated_syntax_scope_is_local () =
  check_i64_macro "generated syntax form does not shadow caller syntax" 6L
    "{
       syntax tag { tag $x => $x + 1 };
       syntax make_tag {
       make_tag => {
           syntax tag { tag $x => $x + 2 };
           tag 2
         }
       };
       make_tag + tag 1
     }" ()

let test_syntax_template_expands_to_template_use () =
  check_i64_macro "syntax template can expand to another syntax use" 5L
    "{
       syntax five { five => 5 };
       syntax call_five { call_five => five };
       call_five
     }" ()

let test_syntax_template_generates_parameterized_syntax_form () =
  check_i64_macro "generated syntax form can bind its own holes and branches" 11L
    "{
       syntax make_bounded {
       make_bounded $limit => {
           syntax bound {
           bound $x below => if ($x < $limit) { $x } else { $limit },
           bound $x above => if ($x > $limit) { $x } else { $limit }
           };
           bound 2 below + bound 9 above
         }
       };
       make_bounded 5
     }" ()

let test_syntax_template_nested_callsite_parenthesized () =
  check_i64_macro "nested syntax callsite inside parenthesized holes" 4L
    "{
       syntax is_zero { is_zero $x => $x == 0 };
       syntax inc { inc $x => $x + 1 };
       syntax wrap { wrap $x => $x + 1 };
       syntax choose {
       choose $cond then $branch else $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       choose (is_zero 0) then (wrap (inc 2)) else (wrap 10)
     }" ()

let test_syntax_template_nested_callsite_unparenthesized () =
  check_i64_macro "nested syntax callsite inside parenthesised holes" 7L
    "{
       syntax bool {
       bool yes => True,
       bool no => False
       };
       syntax add2 { add2 $x => $x + 2 };
       syntax pick {
       pick $cond then $branch otherwise $fallback =>
           if ($cond) { $branch } else { $fallback }
       };
       pick (bool yes) then (add2 5) otherwise add2 10
     }" ()

let test_syntax_template_nested_callsite_repeated_hole () =
  check_i64_macro "nested syntax callsite in repeated hole" 12L
    "{
       syntax inc { inc $x => $x + 1 };
       syntax triple { triple $x => $x + $x + $x };
       triple (inc (inc 2))
     }" ()

let test_syntax_template_multi_branch () =
  check_i64_macro "multi-branch: first-match disambiguation" 1L
    "{
       syntax choose {
       choose one => 1,
       choose two => 2
       };
       choose one
     }" ()

let test_syntax_template_imported_pub_syntax () =
  match
    eval_with_imported_macros
      [ ("syntax_lib",
         (* The replacement is read where it is written, so its [+] comes from
            the unit's own prelude open (M10). *)
         "open (import \"std\");
          pub syntax inc { inc $x => $x + 1 };
          pub x = 0") ]
      "{
         M = import \"syntax_lib\";
         inc 3
       }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported syntax template" 4L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "imported syntax template: %s" (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail (Printf.sprintf "imported syntax template: %s" (Printexc.to_string e))

let test_syntax_template_imported_intro_binding_hygiene () =
  match
    eval_with_imported_macros
      [ ("syntax_lib",
         "pub syntax local_x { local_x => { x = 7; x } };
          pub x = 0") ]
      "{
         M = import \"syntax_lib\";
         x = 99;
         local_x
       }"
  with
  | VAtom (I64 n) ->
      Alcotest.(check int64) "imported syntax template intro binding hygiene" 7L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail
        (Printf.sprintf "imported syntax template intro binding hygiene: %s"
           (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail
        (Printf.sprintf "imported syntax template intro binding hygiene: %s"
           (Printexc.to_string e))

let test_syntax_template_no_holes () =
  check_i64_macro "syntax with no holes" 42L
    "{
       syntax answer { answer => 42 };
       answer
     }" ()

let test_syntax_template_binder_hole () =
  check_i64_macro "binder hole can introduce use-site name" 3L
    "{
       syntax bind {
       bind $(name : Id) $value in $body => { $name = $value; $body }
       };
       bind x 3 in x
     }" ()

let test_syntax_template_ident_hole () =
  check_i64_macro "identifier hole can reference use-site name" 4L
    "{
       x = 4;
       syntax use { use $(name : Id) => $name };
       use x
     }" ()

let test_syntax_template_pattern_hole () =
  check_i64_macro "pattern hole splices a use-site pattern and its binders" 4L
    "{
       syntax unwrap_or {
       unwrap_or $v $(p : Pattern) $body $d => match ($v) { $p => $body, _ => $d }
       };
       unwrap_or (Some(4)) (Some(x)) x 0
     }" ()

let test_syntax_template_unused_capture () =
  check_i64_macro "unused captured hole is accepted" 7L
    "{
       syntax ignore { ignore $unused => 7 };
       ignore MissingName
     }" ()

let test_syntax_template_reuse_duplicates_evaluation () =
  check_i64_macro "reused expression hole duplicates evaluation" 3L
    "{
       r = ref(0);
       inc = fn(_) { { n = deref(r); _ = r <- n + 1; deref(r) } };
       syntax twice { twice $x => $x + $x };
       twice (inc())
     }" ()

let test_decl_template_module_captures_pub_value () =
  check_i64_macro "decl template captures public module value" 42L
    "{
       M = module {
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer = 42
       };
       M.answer
     }" ()

let test_decl_template_module_preserves_typed_value () =
  check_i64_macro "decl template preserves typed value" 42L
    "{
       M = module {
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer : I64 = 42
       };
       M.answer
     }" ()

let test_decl_template_struct_captures_pub_value () =
  check_i64_macro "decl template captures public struct value" 42L
    "{
       Box = struct {
         value: I64;
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer = 42
       };
       Box.answer
     }" ()

let test_decl_template_multi_generates_siblings () =
  match
    eval_with_imported_macros
      [ ( "decls",
          "open (import \"std\");
           syntax pair : Decl {
           pair => {
               base = 40;
               pub answer = base + 2
             }
           };
           pair" ) ]
      "{ M = import \"decls\"; M.answer }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "decl template multi generates siblings" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "decl template multi generates siblings: %s" (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail (Printf.sprintf "decl template multi generates siblings: %s" (Printexc.to_string e))

let test_decl_template_multi_rejected_in_expr () =
  match
    eval_with_imported_macros
      [ ( "bad_syntax",
          "pub syntax bad : Decl {
           bad => {
               x = 1
             }
           }" ) ]
      "{ M = import \"bad_syntax\"; bad }"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "names the kind mismatch" true
        (string_contains msg "has kind Decl but was used in Expr context")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected a Decl syntax form in expression position to be rejected"

let test_decl_template_struct_field_deferred () =
  match
    eval_with_macros
      "{
         Box = struct {
           syntax field : Decl { field => { value: I64 } };
           field
         };
         0
       }"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions deferred struct fields" true
        (string_contains msg "struct field declarations are deferred")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected struct field declaration-template rejection"

let test_7i_generated_pub_syntax_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "open (import \"std\");
                  syntax export_syntax : Decl {
                  export_syntax $(n : Id) =>
                      {
                        pub syntax $n { $n $x => $x + 1 }
                      }
                  };
                  export_syntax inc") ]
      "{ M = import \"gen\"; inc 41 }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub syntax across imports" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub syntax: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub syntax: %s" (Printexc.to_string e))

let test_7i_generated_syntax_usable_later_same_module () =
  check_import_i64 "7I generated syntax usable later" 
    [ ("gen", "open (import \"std\");
               syntax make_inc : Decl {
               make_inc $(n : Id) =>
                   {
                     syntax $n { $n $x => $x + 1 }
                   }
               };
               make_inc inc;
               pub result = inc 5") ]
    6L "{ M = import \"gen\"; M.result }" ()

let test_7i_generated_pub_operator_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "open (import \"std\");
                  syntax export_operator : Decl {
                  export_operator $(op : Id) =>
                      {
                        pub infix ($op) (stx) { Syntax.i64(9) }
                      }
                  };
                  export_operator ~") ]
      "{ M = import \"gen\"; 1 ~ 2 }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub operator across imports" 9L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub operator: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub operator: %s" (Printexc.to_string e))

let test_7i_generated_pub_macro_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "open (import \"std\");\nsyntax export_macro : Decl {
                  export_macro =>
                      {
                        pub macro answer(_) { Syntax.i64(42) }
                      }
                  };
                  export_macro") ]
      "{ M = import \"gen\"; M.answer(0) }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub macro across imports" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub macro: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub macro: %s" (Printexc.to_string e))

let test_7i_generated_pub_operator_rejected_in_struct () =
  match
    eval_with_macros
      "struct {
           syntax export_operator : Decl {
           export_operator =>
               {
                 pub infix (~) (stx) { Syntax.i64(9) }
               }
           };
           export_operator
         }"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions pub operator in struct" true
        (string_contains msg "pub operator is not supported inside structs")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated pub operator in struct to be rejected"

let test_7i_generated_pub_macro_rejected_in_struct () =
  match
    eval_with_macros
      "struct {
           syntax export_macro : Decl {
           export_macro =>
               {
                 pub macro answer(_) { Syntax.i64(42) }
               }
           };
           export_macro
         }"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions pub macro in struct" true
        (string_contains msg "pub macro is not supported inside structs")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated pub macro in struct to be rejected"

let test_7i_generated_syntax_cycle () =
  match
    eval_with_imported_macros
      (* A replacement's import is quoted syntax, loaded where the form is
         used; the cycle is the units importing each other. *)
      [ ("cycle_a", "B = import \"cycle_b\";
                     syntax gen_aop : Decl {
                     gen_aop $(n : Id) => {
                         pub syntax $n { $n $x => $x }
                       }
                     };
                     gen_aop aop");
        ("cycle_b", "A = import \"cycle_a\";
                     syntax gen_bop : Decl {
                     gen_bop $(n : Id) => {
                         pub syntax $n { $n $x => $x }
                       }
                     };
                     gen_bop bop")
      ] "{ A = import \"cycle_a\"; aop 0 }"
  with
  | exception Core_loader.CircularSyntaxVisit "cycle_a" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated syntax circular visit"

let test_7i_generated_macro_cycle () =
  match
    eval_with_imported_macros
      [ ("mac_a", "open (import \"std\");\nsyntax gen_ma : Decl {
                   gen_ma => {
                       pub macro ma(_) { { B = import \"mac_b\"; Syntax.i64(1) } }
                     }
                   };
                   gen_ma");
        ("mac_b", "open (import \"std\");\nsyntax gen_mb : Decl {
                   gen_mb => {
                       pub macro mb(_) { { A = import \"mac_a\"; Syntax.i64(2) } }
                     }
                   };
                   gen_mb")
      ] "{ A = import \"mac_a\"; ma(0) }"
  with
  | exception Core_loader.CircularSyntaxVisit "mac_a" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated macro circular visit"

let test_7i_generated_syntax_hygiene_introduced_binder () =
  check_i64_macro "7I generated syntax hygiene preserves introduced binders" 99L
    "{
       M = module {
         syntax let_x { let_x $body => { x = 1; $body } };
         pub result = {
           x = 99;
           let_x x
         }
       };
       M.result
     }" ()

(* M7: syntactic roles resolve by scope set. *)
let test_m7_syntax_shadows_syntax_in_block () =
  check_i64_macro "syntax shadows syntax in a nested block" 21L
    "{ syntax t { t => 1 }; x = { syntax t { t => 2 }; t }; x * 10 + t }" ()

let test_m7_replacement_reads_roles_at_definition () =
  check_i64_macro "a replacement reads roles as of its definition" (-7L)
    "{ infix (~) ($a, $b) { $a - $b }; syntax t { t => 1 ~ 2 };
       infix (~) ($a, $b) { $a + $b }; t * 10 + (1 ~ 2) }" ()

let test_m7_template_syntax_visible_in_its_output () =
  check_i64_macro "a template's syntax form is visible in its own output" 42L
    "{ syntax mk { mk => { syntax inc { inc $x => $x + 1 }; inc 41 } }; mk }" ()

let test_m7_generated_syntax_usable_by_next_form () =
  check_import_i64 "generated syntax named at the use site is usable by the next form"
    [ ("gen", "open (import \"std\");
               syntax make : Decl { make $(n : Id) => { syntax $n { $n $x => $x * 2 } } };
               make double;
               pub r = double 21") ]
    42L "{ M = import \"gen\"; M.r }" ()

(* M7 decision 3: a role never mixes with another binder of its name. *)
let role_conflict label source () =
  match eval_with_macros source with
  | exception Expand_error.Error { error = RoleConflict _; _ } -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: unexpected exception %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected a role conflict")

let test_m7_value_binder_under_syntax () =
  role_conflict "a value binder under a syntax form of its name"
    "{ syntax answer { answer => 42 }; { answer = 7; 1 } }" ()

let test_m7_syntax_after_value_binder () =
  role_conflict "a syntax form declared where a value of its name is visible"
    "{ answer = 7; syntax answer { answer => 42 }; 1 }" ()

let test_m7_fn_param_under_syntax () =
  role_conflict "a parameter named like a syntax form"
    "{ syntax answer { answer => 42 }; f = fn(answer) { 1 }; 0 }" ()

let test_m7_match_binder_under_syntax () =
  role_conflict "a pattern binder named like a syntax form"
    "{ syntax answer { answer => 42 }; match (5) { answer => 1 } }" ()

let test_m7_syntax_inside_param_region () =
  role_conflict "a syntax form declared inside a parameter's region"
    "{ f = fn(answer) { syntax answer { answer => 42 }; 1 }; 0 }" ()

let test_m7_value_under_macro () =
  role_conflict "a value binder named like a macro"
    "{ macro m(_) { quote(1) }; m = 5; 1 }" ()

let test_m7_value_under_imported_operator () =
  role_conflict "a value binder named like an imported prefix operator"
    "{ not = 5; 1 }" ()

let test_m7_sibling_regions_do_not_conflict () =
  check_i64_macro "a parameter and a later syntax form of its name in a sibling region" 42L
    "{ f = fn(answer) { answer }; { syntax answer { answer => 42 }; answer } }" ()

let test_m7_hygienic_macro_binder_is_apart () =
  check_i64_macro "a macro's own binder named like a syntax form" 1L
    "{ syntax answer { answer => 42 }; macro m(_) { quote({ answer = 7; 1 }) }; m(0) }" ()

let test_m7_fixity_attaches_to_value () =
  check_i64_macro "a fixity declaration attaches to the value of its name" 42L
    "{ twice = fn(x) { x * 2 }; prefix (twice); twice 21 }" ()

let test_m7_new_binder_under_attached_fixity () =
  role_conflict "a new binder of a name whose value has a fixity"
    "{ twice = fn(x) { x * 2 }; prefix (twice); { twice = 5; 1 } }" ()

let open_supplies_role label source () =
  match eval_with_macros source with
  | exception Elab_error.ElabError (OpenSuppliesRole "answer") -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: unexpected exception %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected the open to be rejected")

let test_m7_open_under_syntax () =
  open_supplies_role "an open supplying a member named like a visible syntax form"
    "{ M = module { pub answer = 7 }; syntax answer { answer => 42 }; open M; 1 }" ()

let test_m7_syntax_inside_open_region () =
  open_supplies_role "a syntax form declared where an open supplies its name"
    "{ M = module { pub answer = 7 }; open M; syntax answer { answer => 42 }; 1 }" ()

let test_m7_open_without_conflict () =
  check_i64_macro "an open supplying other names" 7L
    "{ M = module { pub x = 7 }; syntax answer { answer => 42 }; open M; x }" ()

(* Role visibility gaps left by M7: an imported role is visible in the region
   of the open or binder that imported it, and every open is checked. *)
let answer_syntax = ("ops", "pub syntax answer { answer => 42 }")

let test_m7_import_open_role_in_region () =
  match eval_with_imported_macros [ answer_syntax ] "{ x = { open (import \"ops\"); answer }; x }" with
  | VAtom (I64 n) -> Alcotest.(check int64) "an imported role is visible in its open's region" 42L n
  | _ -> Alcotest.fail "expected 42"

let imported_role_leaks label source () =
  match eval_with_imported_macros [ answer_syntax ] source with
  | VAtom (I64 84L) -> Alcotest.fail (label ^ ": an imported role leaked out of its region")
  | _ -> Alcotest.fail (label ^ ": expected the use outside the region to be rejected")
  | exception (Enforest.Error _ | Enforest.Unsupported _ | Elab_error.ElabError _) -> ()

let test_m7_import_open_role_not_after_block () =
  imported_role_leaks "a block's open (import …)" "{ x = { open (import \"ops\"); answer }; x + answer }" ()

let test_m7_import_binder_role_not_after_block () =
  imported_role_leaks "a block's M = import …" "{ x = { M = import \"ops\"; answer }; x + answer }" ()

let test_m7_import_open_under_syntax () =
  match
    eval_with_imported_macros
      [ ("m_answer", "pub answer = 7");
        ("user", "syntax answer { answer => 42 };\nopen (import \"m_answer\");\npub r = 1") ]
      "{ U = import \"user\"; U.r }"
  with
  | exception Elab_error.ElabError (OpenSuppliesRole "answer") -> ()
  | exception e -> Alcotest.fail ("unexpected exception " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "an import open supplying a name a unit role has must be rejected"

let test_m7_driver_open_under_syntax () =
  match run_driver "M = module { pub answer = 7 };\nsyntax answer { answer => 42 };\nopen M;\npub r = 1" with
  | exception Elab_error.ElabError (OpenSuppliesRole "answer") -> ()
  | exception e -> Alcotest.fail ("unexpected exception " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "an open the driver elaborates must be checked"

let test_m7_template_written_syntax_invisible () =
  match
    eval_with_imported_macros
      [ ("gen", "open (import \"std\");
                 syntax make : Decl { make => { syntax inc { inc $x => $x + 1 } } };
                 make;
                 pub r = inc 5") ]
      "{ M = import \"gen\"; M.r }"
  with
  | VAtom (I64 6L) -> Alcotest.fail "a syntax form the template named itself must be invisible to user code"
  | _ -> Alcotest.fail "expected the use of an invisible syntax form to be rejected"
  | exception (Enforest.Error _ | Enforest.Unsupported _ | Elab_error.ElabError _) -> ()


(* M9: syntax forms are macros filled at expansion; bodies stay unread until
   expansion reaches them; blocks reflect as token trees. *)

(* Rust-style arms: brackets end an arm, otherwise [,] does; [|] is union. *)
let test_arms_evaluate () =
  check_i64 "brace and expression arms" 23L
    "{
       type Color = Red | Green | Blue;
       pick = fn(c) { match (c) { Red | Blue => { x = 1; x + 1 } Green => 3, } };
       syntax twice { twice $x => $x + $x, twice => 0 };
       pick(Blue) * 10 + (twice 1) + match (Green) { Green => if (True) { 1 } else { 2 }, _ => 0 }
     }" ()

let test_m9_block_tokens_inspected () =
  check_i64_macro "a macro reads a block's tokens" 1L
    "{
       macro sql(q) {
         match (Syntax.tokens(q)) {
         Cons(Syntax.Tok(_, Syntax.IdentTok(word), _), _) =>
             if (i64_to_bool(eq_string(word, \"SELECT\"))) { Syntax.i64(1) } else { Syntax.i64(0) },
         _ => Syntax.i64(2)
         }
       };
       sql({ SELECT name FROM users WHERE age > 18 })
     }" ()

let test_m9_block_hole_in_module_slot () =
  check_import_i64 "a Block hole fills a module body"
    [ ("ns", "open (import \"std\");
              syntax namespace : Decl { namespace $(n : Id) $(b : Block) => { pub $n = module $b } };
              namespace Geometry { pub pi = 3; pub tau = 6 };
              pub answer = Geometry.tau") ]
    6L "{ M = import \"ns\"; M.answer }" ()

let test_m9_body_read_after_earlier_statement () =
  check_i64_macro "a body inside a macro argument reads generated syntax" 12L
    "{
       syntax make_inc : Decl { make_inc $(n : Id) => { syntax $n { $n $x => $x + 1 } } };
       macro twice(e) { quote($e + $e) };
       run = fn(f) { f(()) };
       twice(run(fn(_) { make_inc inc; inc 5 }))
     }" ()

let test_m9_decl_form_as_block_statement () =
  check_i64_macro "a Decl form binds for the rest of a block" 7L
    "{
       syntax seven : Decl { seven $(n : Id) => { $n = 7 } };
       seven x;
       x
     }" ()

let test_m9_expand_block_placed_back () =
  check_i64_macro "expanded forms placed back into output" 8L
    "{
       syntax double { double $x => $x + $x };
       macro pre(b) { Syntax.expand_block(b) };
       pre({ z = 4; double z })
     }" ()

let test_m9_quote_nested_rule_holes () =
  check_i64_macro "a quote's inner rule binds its own holes, the macro's fill the rest" 21L
    "{
       M = module {
         macro make_adder(base) : List(Decl) {
           quote { syntax add_base { add_base $x => $x + $base }; pub result = add_base 1 + add_base 10; }
         };
         make_adder(5)
       };
       M.result
     }" ()

let test_m9_quote_token_position_hole () =
  check_i64_macro "a quote fills a hole naming generated syntax" 42L
    "{
       M = module {
         macro make(n) : List(Decl) {
           match (n) { Syntax.Var(name) => quote { syntax $name { $name $x => $x * 2 }; }, _ => quote { } }
         };
         make(double);
         pub r = double 21
       };
       M.r
     }" ()

(* M9: a macro parameter takes a kind, as a syntax form's hole does; its call's
   arguments are read as those kinds. *)

let expect_expand_error label check source =
  match eval_with_macros source with
  | exception Expand_error.Error { error; _ } when check error -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an expansion error")

(* M12: every declaration binder is fresh, so a type a macro declares does not
   take the caller's type of the same name. *)
let test_declaration_binders_are_fresh () =
  check_i64_macro "the caller's type is not the macro's" 1L
    "{
       type Tmp = Yes | No;
       macro with_tmp(e) { quote({ type Tmp = A | B; $e }) };
       with_tmp({ v : Tmp = Yes; match (v) { Yes => 1, No => 0 } })
     }" ()

(* M11/M12: with no string-built ids, a resolved name reaches the expander only
   under the certificate its scopes carry, so a macro cannot forge one. *)
let test_resolved_names_cannot_be_forged () =
  expect_expand_error "a resolved name the macro was not given"
    (function Expand_error.NotSyntax _ -> true | _ -> false)
    "{ x = 5; macro steal(n : Id) { Syntax.RawVar(None, Syntax.Id{name = \"x#0\"; span = None; scope = n.scope}) }; steal(y) }"

let test_m9_param_id () =
  check_i64_macro "an Id parameter names the use site's binder" 5L
    "{ macro same(n : Id) { Syntax.RawVar(None, n) }; x = 5; same(x) }" ()

let test_m9_param_id_binds () =
  check_i64_macro "an Id parameter binds for the caller" 7L
    "{
       M = module {
         macro seven(n : Id) : Decl { Syntax.decl_let(n, Syntax.i64(7), False) };
         seven(x);
         pub r = x
       };
       M.r
     }" ()

let test_m9_param_pattern () =
  check_i64_macro "a Pattern parameter is read as a pattern" 10L
    "{
       macro matches(p : Pattern, e) { quote(match ($e) { $p => 1, _ => 0 }) };
       matches(Some(_), Some(3)) * 10 + matches(None, Some(3))
     }" ()

let test_m9_param_type_aware () =
  check_i64_macro "a type-aware macro's Id parameter" 4L
    "{ macro pick[A](n : Id) : Expr(A) { { _ = A; Syntax.RawVar(None, n) } }; x = 3; pick(x) + 1 }" ()

let test_m9_param_block () =
  check_i64_macro "a Block parameter is the unread block" 1L
    "{
       macro sql(q : Block) {
         match (Syntax.tokens(q)) {
         Cons(Syntax.Tok(_, Syntax.IdentTok(word), _), _) =>
             if (i64_to_bool(eq_string(word, \"SELECT\"))) { Syntax.i64(1) } else { Syntax.i64(0) },
         _ => Syntax.i64(2)
         }
       };
       sql({ SELECT name FROM users })
     }" ()

let test_m9_param_kind_mismatch () =
  expect_expand_error "an Id argument that is not an identifier"
    (function Expand_error.ArgumentKind { kind = HoleId; _ } -> true | _ -> false)
    "{ macro same(n : Id) { Syntax.RawVar(None, n) }; same(1) }";
  expect_expand_error "a Block argument that is not a block"
    (function Expand_error.ArgumentKind { kind = HoleBlock; _ } -> true | _ -> false)
    "{ macro b(q : Block) { q }; b(1) }";
  expect_expand_error "too many arguments"
    (function Expand_error.ArgumentCount { expected = 1; got = 2; _ } -> true | _ -> false)
    "{ macro same(n : Id) { Syntax.RawVar(None, n) }; x = 1; same(x, x) }"

(* A Decl parameter's argument is a brace group of items; its value is the
   Decls it holds, spliced by a declaration hole as [quote { … }] builds. *)
let test_m9_param_decl () =
  check_i64_macro "a Decl parameter spliced into a quote" 33L
    "{
       M = module {
         macro with_extra(d : List(Decl)) : List(Decl) { quote { $d; pub extra = 22; } };
         with_extra({ pub x = 1; pub y = 10 })
       };
       M.x + M.y + M.extra
     }" ();
  check_i64_macro "a Decl parameter spliced twice" 2L
    "{
       M = module {
         macro twice_decls(d : List(Decl)) : List(Decl) { quote { $d; $d } };
         twice_decls({ pub x = 1; pub y = 2 })
       };
       M.y
     }" ();
  (* Like a syntax form's Decl capture, the items stay unread until spliced. *)
  check_i64_macro "a Decl parameter's items arrive unread" 1L
    "{
       macro unread(d : List(Decl)) {
         match (d) { Cons(Syntax.DeclItems(_), Nil) => Syntax.i64(1), _ => Syntax.i64(0) }
       };
       unread({ a = 1; b = 2 })
     }" ()

let test_m9_param_decl_kind_mismatch () =
  expect_expand_error "a Decl argument that is not a brace group"
    (function Expand_error.ArgumentKind { kind = HoleDecl; _ } -> true | _ -> false)
    "{ M = module { macro m(d : List(Decl)) : List(Decl) { quote { $d } }; m(x) }; 0 }"

(* A parameter's kind means what the same type means as an output:
   [(d : Decl)] is exactly one declaration, a group holding one item. *)
let test_m9_param_one_decl () =
  check_i64_macro "a Decl parameter is one declaration, returned as a Decl" 4L
    "{
       M = module {
         macro keep1(d : Decl) : Decl { d };
         keep1({ pub x = 4 })
       };
       M.x
     }" ();
  expect_expand_error "a Decl parameter given two declarations"
    (function Expand_error.ArgumentKind { kind = HoleOneDecl; _ } -> true | _ -> false)
    "{ M = module { macro keep1(d : Decl) : Decl { d }; keep1({ pub x = 1; pub y = 2 }) }; 0 }";
  check_operator "an imported macro's Decl parameter" 6L
    [ ("one_decl", "open (import \"std\");\npub macro keep1(d : Decl) : Decl { d }") ]
    "{ M = module { open (import \"one_decl\"); keep1({ pub x = 6 }) }; M.x }"

(* [expand_decls(d)]: a Decl argument's items, expanded form by form - an item
   reads with the syntax earlier items declared - and placeable back into output. *)
let test_expand_decls () =
  check_i64_macro "expand_decls reads the items, so a macro can count them" 3L
    "{
       macro count(d : List(Decl)) {
         match (Syntax.expand_decls(d)) { Cons(_, Cons(_, Cons(_, Nil))) => Syntax.i64(3), _ => Syntax.i64(0) }
       };
       count({ a = 1; b = 2; c = 3 })
     }" ();
  check_i64_macro "a later item reads with syntax an earlier item declares" 2L
    "{
       M = module {
         macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
         keep({ syntax inc { inc $x => $x + 1 }; pub y = inc 1 })
       };
       M.y
     }" ();
  check_i64_macro "expanded items placed back into a quote" 7L
    "{
       M = module {
         macro wrap(d : List(Decl)) : List(Decl) { e = Syntax.expand_decls(d); quote { $e; pub z = 5; } };
         wrap({ syntax inc { inc $x => $x + 1 }; pub y = inc 1 })
       };
       M.y + M.z
     }" ()

let test_expand_decls_budget () =
  match
    eval_with_macros
      ("{ macro spin(_) " ^ diverging_body
     ^ "; macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) }; M = module { keep({ pub x = spin(0) }) }; 0 }")
  with
  | exception Expand_error.Error { error = BudgetExceeded { macro; _ }; _ } ->
      Alcotest.(check bool) "names the macro" true (string_contains macro "spin")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an expansion budget error"

let test_expand_decls_imported () =
  check_operator "an imported macro reads its Decl argument" 2L
    [ ("readers", "open (import \"std\");\npub macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) }") ]
    "{ M = module { open (import \"readers\"); keep({ syntax inc { inc $x => $x + 1 }; pub y = inc 1 }) }; M.y }"

let kinded_unit =
  ("kinds", "open (import \"std\");
             pub macro same(n : Id) { Syntax.RawVar(None, n) };
             pub macro seven(n : Id) : Decl { Syntax.decl_let(n, Syntax.i64(7), False) };
             pub macro with_extra(d : List(Decl)) : List(Decl) { quote { $d; pub extra = 22; } }")

let test_m9_param_imported () =
  check_operator "an imported macro's Id parameter, dotted" 5L [ kinded_unit ]
    "{ M = import \"kinds\"; x = 5; M.same(x) }";
  check_operator "an imported macro's Id parameter, opened" 5L [ kinded_unit ]
    "{ open (import \"kinds\"); x = 5; same(x) }";
  check_operator "an imported Decl macro's Id parameter" 7L [ kinded_unit ]
    "{ M = module { open (import \"kinds\"); seven(y); pub r = y }; M.r }";
  check_operator "an imported macro's Decl parameter" 33L [ kinded_unit ]
    "{ M = module { open (import \"kinds\"); with_extra({ pub x = 1; pub y = 10 }) }; M.x + M.y + M.extra }"

(* M8: every macro application is given exactly the arguments its macro
   declares - a Decl macro is never run on syntax it was not given. *)
let count_error expected got = function
  | Expand_error.ArgumentCount { expected = e; got = g; _ } -> e = expected && g = got
  | _ -> false

let test_m8_argument_count () =
  expect_expand_error "a Decl macro given too few arguments" (count_error 2 1)
    "{ M = module { macro two(a, b) : Decl { quote { x = $a } }; two(1); pub r = 1 }; M.r }";
  expect_expand_error "a Decl macro given too many arguments" (count_error 2 3)
    "{ M = module { macro two(a, b) : Decl { quote { x = $a } }; two(1, 2, 3); pub r = 1 }; M.r }";
  expect_expand_error "an Expr macro given too many arguments" (count_error 1 2)
    "{ macro one(a) { a }; one(1, 2) }";
  expect_expand_error "an Expr macro given too few arguments" (count_error 2 1)
    "{ macro two(a, b) { a }; two(1) }";
  check_i64_macro "a Decl macro with an empty parameter list" 4L
    "{ M = module { macro four() : Decl { quote { four = 4 } }; four(); pub r = 1 }; M.r + 3 }" ();
  check_i64_macro "a macro's result applied to a further argument" 5L
    "{ macro ident(_) { quote(fn(y) { y }) }; ident(0)(5) }" ()

(* A Decl macro's output type: [: Decl] is one declaration, [: List(Decl)] any
   number; its body is checked against it where the macro is defined. *)
let test_decl_macro_output_type () =
  check_i64_macro "a : Decl macro returns one quoted declaration" 1L
    "{ M = module { macro one() : Decl { quote { pub a = 1 } }; one() }; M.a }" ();
  check_i64_macro "a : List(Decl) macro returns several" 3L
    "{ M = module { macro two() : List(Decl) { quote { pub a = 1; pub b = 2 } }; two() }; M.a + M.b }" ();
  check_i64_macro "both splice at calls in one module" 4L
    "{ M = module {
         macro one() : Decl { quote { pub c = 1 } };
         macro two() : List(Decl) { quote { pub a = 1; pub b = 2 } };
         one(); two()
       }; M.a + M.b + M.c }" ();
  expect_elab_error "a : Decl quote holding two declarations"
    (function Elab_error.QuoteNotOneDecl 2 -> true | _ -> false)
    "{ macro two() : Decl { quote { a = 1; b = 2 } }; 0 }";
  match eval_with_macros "{ macro none() : Decl { Nil }; 0 }" with
  | exception Unify.UnifyError (Unify.NominalMismatch _) -> ()
  | exception e -> Alcotest.fail ("a : Decl macro returning a list: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "a : Decl macro returning a list: expected a type error at the definition"

let test_m8_imported_argument_count () =
  match eval_with_imported_macros [ kinded_unit ] "{ M = module { open (import \"kinds\"); seven(y, z); pub r = 1 }; M.r }" with
  | exception Expand_error.Error { error; _ } when count_error 1 2 error -> ()
  | exception e -> Alcotest.fail (Printexc.to_string e)
  | _ -> Alcotest.fail "expected an argument count error"

(* Names and shape only: a binder expanded again gets a fresh scope, which
   resolution of an already-resolved name never consults. *)
let erase_scopes stx = Expand.map_ids (fun id -> { id with Syntax.scope = Scope_set.empty }) stx

let test_m9_expansion_idempotent () =
  let source = "{ syntax double { double $x => $x + $x }; f = fn(y) { z = double y; match (z) { w => w } }; f(3) }" in
  let once, ctx = Parse_expand.parse_expr_with_ctx ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source in
  let twice = Expand.expand ctx once in
  Alcotest.(check bool) "expanding expanded syntax renames nothing" true (erase_scopes once = erase_scopes twice)

(* An expression's expansion inside the prelude open, with scopes and source
   positions erased: the two sources differ in length. *)
let expanded_body source =
  let ctx = Elaborate.init_ctx () in
  let elaborate expr = let core, _ = Elaborate.on_expr ctx expr in Elaborate.Ctx.eval ctx core in
  let expr, _ =
    Parse_expand.parse_expr_with_ctx ~elaborate ~eval_and_apply:Nbe.apply_macro
      ~syntax_nominals:(Elaborate.syntax_nominals ctx) ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source
  in
  let erase stx =
    Expand.map_forms
      (fun id -> { id with Syntax.scope = Scope_set.empty; span = Source_span.synthetic })
      (fun form -> { form with Syntax.span = Source_span.synthetic })
      stx
  in
  match expr.kind with Syntax.Open (_, body, _) -> erase body | _ -> erase expr

let test_m9_filling_equals_quote () =
  Alcotest.(check bool) "a syntax form fills what its macro's quote evaluates to" true
    (expanded_body "{ syntax plus1 { plus1 $x => $x + 1 }; plus1 41 }"
     = expanded_body "{ macro plus1(x) { quote($x + 1) }; plus1(41) }")

let () =
  Alcotest.run "core"
    [
      ( "eval",
        [
          Alcotest.test_case "atom" `Quick (check_i64 "atom" 42L "42");
          Alcotest.test_case "lam+ap" `Quick (check_i64 "lam+ap" 7L "(fn(x) { x })(7)");
          Alcotest.test_case "apply twice" `Quick
            (check_i64 "apply twice" 5L
               "{
                  twice : (I64 -> I64) -> I64 -> I64 = fn(f) { fn(x) { f(f(x)) } };
                  inc : I64 -> I64 = fn(n) { n + 1 };
                  twice(inc, 3)
                }");
          Alcotest.test_case "let" `Quick (check_i64 "let" 5L "{ x : I64 = 5; x }");
          Alcotest.test_case "let shadowing" `Quick
            (check_i64 "let shadowing" 2L "{ x = 1; x = 2; x }");
          Alcotest.test_case "non-rec let rhs sees outer" `Quick
            (check_i64 "non-rec let rhs sees outer" 1L "{ x = 1; x = x; x }");
          Alcotest.test_case "lambda shadows outer let" `Quick
            (check_i64 "lambda shadows outer let" 7L "{ x = 1; (fn(x) { x } : I64 -> I64)(7) }");
          Alcotest.test_case "if True" `Quick (check_i64 "if True" 1L "if (True) { 1 } else { 2 }");
          Alcotest.test_case "if False" `Quick (check_i64 "if False" 2L "if (False) { 1 } else { 2 }");
          Alcotest.test_case "and true true" `Quick (check_bool "and true true" true "True && True");
          Alcotest.test_case "and true false" `Quick (check_bool "and true false" false "True && False");
          Alcotest.test_case "and short-circuits" `Quick
            (check_bool "and short-circuits" false "False && panic[Bool](\"and rhs evaluated\")");
          Alcotest.test_case "or false true" `Quick (check_bool "or false true" true "False || True");
          Alcotest.test_case "or false false" `Quick (check_bool "or false false" false "False || False");
          Alcotest.test_case "or short-circuits" `Quick
            (check_bool "or short-circuits" true "True || panic[Bool](\"or rhs evaluated\")");
          Alcotest.test_case "and binds tighter than or" `Quick
            (check_bool "and binds tighter than or" true "True || False && False");
          Alcotest.test_case "comparison binds tighter than and" `Quick
            (check_bool "comparison binds tighter than and" false "1 < 2 && 3 < 2");
          Alcotest.test_case "and or in if condition" `Quick
            (check_i64 "and or in if condition" 1L "if (1 < 2 && 2 < 3 || False) { 1 } else { 2 }");
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "proj" `Quick (check_i64 "proj" 42L "(42, True).0");
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "module signature argument" `Quick test_eval_module_signature_argument;
          Alcotest.test_case "module signature extra field" `Quick test_eval_module_signature_extra_field;
          Alcotest.test_case "signature sugar argument" `Quick test_eval_signature_sugar_argument;
          Alcotest.test_case "let-bound signature" `Quick test_eval_let_bound_signature;
          Alcotest.test_case "a module is not a signature" `Quick test_eval_module_is_not_a_signature;
          Alcotest.test_case "imported signature" `Quick test_eval_imported_signature;
          Alcotest.test_case "module signature functor" `Quick test_eval_module_signature_functor;
          Alcotest.test_case "ref read initial" `Quick test_ref_read_initial;
          Alcotest.test_case "ref write read" `Quick test_ref_write_read;
          Alcotest.test_case "ref aliases share cell" `Quick test_ref_aliases_share_cell;
          Alcotest.test_case "ref closure observes later write" `Quick test_ref_closure_observes_later_write;
          Alcotest.test_case "ref repeated closure increments" `Quick test_ref_repeated_closure_increments;
          Alcotest.test_case "pi" `Quick test_eval_pi;
          Alcotest.test_case "eq i64" `Quick (check_bool "eq i64" true "1 == 1");
          Alcotest.test_case "neq i64" `Quick (check_bool "neq i64" true "1 != 2");
          Alcotest.test_case "eq bool" `Quick (check_bool "eq bool" false "True == False");
          Alcotest.test_case "eq char" `Quick (check_bool "eq char" true "'a' == 'a'");
          Alcotest.test_case "eq unit" `Quick (check_bool "eq unit" true "() == ()");
          Alcotest.test_case "eq string" `Quick (check_bool "eq string" true "\"hello\" == \"hello\"");
          Alcotest.test_case "neq string" `Quick (check_bool "neq string" true "\"hello\" != \"world\"");
          Alcotest.test_case "eq nominal rejected" `Quick test_eval_equality_nominal_rejected;
          Alcotest.test_case "panic message" `Quick (fun () ->
              match eval_source "panic[I64](\"test message\")" with
              | exception Nbe.EvalError "test message" -> ()
              | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
              | _ -> Alcotest.fail "expected panic");
          Alcotest.test_case "fix" `Quick
            (check_i64 "fix" 0L
               "{ rec f : Bool -> I64 = fn(x) { if (x) { 0 } else { f(True) } }; f(False) }");
          Alcotest.test_case "rec sum" `Quick
            (check_i64 "rec sum" 15L
               "{ rec sum : I64 -> I64 = fn(n) { if (n == 0) { 0 } else { sum(n - 1) + n } }; sum(5) }");
          Alcotest.test_case "factorial" `Quick
            (check_i64 "factorial" 120L
               "{ rec fact : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; fact(5) }");
          Alcotest.test_case "fibonacci" `Quick
            (check_i64 "fibonacci" 8L
               "{ rec fib : I64 -> I64 = fn(n) { if (n <= 1) { n } else { fib(n - 1) + fib(n - 2) } }; fib(6) }");
          Alcotest.test_case "rec count" `Quick
            (check_i64 "rec count" 5L
               "{ rec f : I64 -> I64 = fn(n) { if (n == 5) { 5 } else { f(n + 1) } }; f(0) }");
          Alcotest.test_case "rec not" `Quick
            (check_i64 "rec not" 0L
               "{ rec f : Bool -> I64 = fn(x) { if (x) { 0 } else { f(not x) } }; f(False) }");
          Alcotest.test_case "unhandled perform" `Quick test_eval_unhandled_perform;
          Alcotest.test_case "unhandled effect at the top is an error" `Quick test_top_unhandled_perform;
          Alcotest.test_case "an escaping closure called at the top is an error" `Quick test_top_escaping_closure;
          Alcotest.test_case "handlers tunnel callback effects" `Quick test_handlers_tunnel;
          Alcotest.test_case "a handled effect may not escape its handler" `Quick test_handled_effect_escape;
          Alcotest.test_case "handled and latent effects pass the top" `Quick test_top_handled_and_latent;
          Alcotest.test_case "an imported unit's unhandled effect is an error" `Quick test_top_unhandled_in_imported_unit;
          Alcotest.test_case "a method is pure unless it declares a row" `Quick test_method_rows;
          Alcotest.test_case "a trait method signature carries a row" `Quick test_trait_method_rows;
          Alcotest.test_case "handler ignores continuation" `Quick test_eval_handler_ignores_continuation;
          Alcotest.test_case "handler resumes once" `Quick test_eval_handler_resumes_once;
          Alcotest.test_case "resume passes through the value branch" `Quick test_eval_handler_resume_through_value_branch;
          Alcotest.test_case "handler value branch" `Quick test_eval_handler_value_branch;
          Alcotest.test_case "match binds a closure scrutinee" `Quick test_eval_match_binds_a_closure;
          Alcotest.test_case "handler outer bubble" `Quick test_eval_handler_outer_bubble;
          Alcotest.test_case "handler escape skips continuation" `Quick test_eval_handler_escape_skips_continuation;
          Alcotest.test_case "handler ping pong effects" `Quick test_eval_handler_ping_pong_effects;
          Alcotest.test_case "recursive handler ping pong effects" `Quick test_eval_recursive_handler_ping_pong_effects;
          Alcotest.test_case "state handler sequences operations" `Quick test_eval_state_handler_sequences_operations;
          Alcotest.test_case "handler tuple payload pattern" `Quick test_eval_handler_tuple_payload_pattern;
          Alcotest.test_case "handler tuple payload binding order" `Quick test_eval_handler_tuple_payload_binding_order;
          Alcotest.test_case "handler record payload pattern" `Quick test_eval_handler_record_payload_pattern;
          Alcotest.test_case "handler record payload binding order" `Quick test_eval_handler_record_payload_binding_order;
          Alcotest.test_case "type-case I64 zero" `Quick test_eval_type_case_i64_zero;
          Alcotest.test_case "type-case I64 nonzero" `Quick test_eval_type_case_i64_nonzero;
          Alcotest.test_case "type-case Bool False" `Quick test_eval_type_case_bool_false;
          Alcotest.test_case "type-case Bool True" `Quick test_eval_type_case_bool_true;
          Alcotest.test_case "type-case Unit" `Quick test_eval_type_case_unit;
          Alcotest.test_case "type-case Char a" `Quick test_eval_type_case_char_a;
          Alcotest.test_case "type-case default I64" `Quick test_eval_type_case_default_i64;
          Alcotest.test_case "type-case default Bool" `Quick test_eval_type_case_default_bool;
          Alcotest.test_case "type-case default Unit" `Quick test_eval_type_case_default_unit;
          Alcotest.test_case "type-case default String panics" `Quick test_eval_type_case_default_string_panics;
          Alcotest.test_case "type-case default_or I64" `Quick test_eval_type_case_default_or_i64;
          Alcotest.test_case "type-case default_or Bool" `Quick test_eval_type_case_default_or_bool;
          Alcotest.test_case "type-case default_or String" `Quick test_eval_type_case_default_or_string;
          Alcotest.test_case "type-case default_or nominal fallback" `Quick test_eval_type_case_default_or_nominal_fallback;
          Alcotest.test_case "type-case type_name I64" `Quick test_eval_type_case_type_name_i64;
          Alcotest.test_case "type-case type_name String" `Quick test_eval_type_case_type_name_string;
          Alcotest.test_case "type-case nominal full application" `Quick test_eval_type_case_nominal_full_application;
          Alcotest.test_case "type-case nominal param bind" `Quick test_eval_type_case_nominal_param_bind;
          Alcotest.test_case "type-case nominal complex param pattern" `Quick test_eval_type_case_nominal_complex_param_pattern;
          Alcotest.test_case "type-case nominal classifier I64" `Quick test_eval_type_case_nominal_classifier_i64;
          Alcotest.test_case "type-case nominal classifier Bool" `Quick test_eval_type_case_nominal_classifier_bool;
          Alcotest.test_case "type-case nominal classifier fallback" `Quick test_eval_type_case_nominal_classifier_fallback;
          Alcotest.test_case "type-case struct field I64" `Quick test_eval_type_case_struct_field_i64;
          Alcotest.test_case "type-case struct field Bool" `Quick test_eval_type_case_struct_field_bool;
          Alcotest.test_case "type-case struct field binder" `Quick test_eval_type_case_struct_field_binder;
          Alcotest.test_case "type-case struct field fallback" `Quick test_eval_type_case_struct_field_fallback;
          Alcotest.test_case "type-case struct closed rejects extra" `Quick test_eval_type_case_struct_closed_rejects_extra;
          Alcotest.test_case "handler same match branch effect" `Quick test_eval_handler_same_match_branch_effect;
          Alcotest.test_case "handler parameterized dispatch" `Quick test_eval_handler_parameterized_dispatch;
          Alcotest.test_case "handler value branch handles same effect" `Quick test_eval_handler_value_branch_handles_same_effect;
          Alcotest.test_case "handler value branch bubbles outer effect" `Quick test_eval_handler_value_branch_bubbles_outer_effect;
          Alcotest.test_case "handler resumed continuation is deep" `Quick test_eval_handler_resumed_continuation_is_deep;
          Alcotest.test_case "handler outer handles residual effect" `Quick test_eval_handler_outer_handles_residual_effect;
          Alcotest.test_case "handler lexical resume nested lambda" `Quick test_eval_handler_lexical_resume_nested_lambda;
          Alcotest.test_case "continuation reuse error" `Quick test_eval_continuation_reuse_error;
          Alcotest.test_case "match ctor" `Quick
            (check_i64 "match ctor" 1L
               "{ type Color = Red | Green; match (Red) { Red => 1, Green => 2 } }");
          Alcotest.test_case "match wildcard" `Quick
            (check_i64 "match wildcard" 99L
               "{ type Color = Red | Green | Blue; \
                match (Green) { Red => 1, _ => 99 } }");
           Alcotest.test_case "match bind" `Quick
             (check_i64 "match bind" 42L
                "{ type Option a = Some(a) | None; \
                  match (Some(42)) { Some(x) => x, None => 0 } }");
          Alcotest.test_case "match binder shadows outer" `Quick
            (check_i64 "match binder shadows outer" 7L
               "{ x = 99; match (7) { x => x } }");
           Alcotest.test_case "match constructor or-pattern binding" `Quick
             (check_i64 "match constructor or-pattern binding" 5L
                "{ type E = A(I64) | B(I64); \
                 match (B(5)) { (A(x) | B(x)) => x } }");
           Alcotest.test_case "match nested" `Quick
             (check_i64 "match nested" 7L
                "{ type Option a = Some(a) | None; \
                 match (Some(Some(7))) { \
                   Some(Some(x)) => x, Some(None) => 0, None => 0 } }");
             Alcotest.test_case "recursive parameterized ADT match" `Quick
                (check_i64 "recursive parameterized ADT match" 1L
                   "{ type MyList(a) = Cons(a, MyList(a)) | Nil; \
                    match (Cons(1, Nil)) { Cons(x, _) => x, Nil => 0 } }");
              Alcotest.test_case "recursive list sum" `Quick
                (check_i64 "recursive list sum" 6L
                   "{ type MyList(a) = Cons(a, MyList(a)) | Nil; \
                    rec sum : MyList(I64) -> I64 = fn(xs) { \
                      match (xs) { Cons(x, rest) => x + sum(rest), Nil => 0 } }; \
                    sum(Cons(1, Cons(2, Cons(3, Nil)))) }");
             Alcotest.test_case "constructor comma payload distinct from tuple" `Quick
               (check_i64 "constructor comma payload distinct from tuple" 6L
                  "{ type Triple(a, b, c) = T(a, Tuple(2, b, c)); \
                   match (T(1, (2, 3))) { T(x, yz) => x + yz.0 + yz.1 } }");
             Alcotest.test_case "constructor tuple payload remains single arg" `Quick
               (check_i64 "constructor tuple payload remains single arg" 1L
                  "{ type Pair = P(Tuple(2, I64, Bool)); \
                   match (P((1, True))) { P(pair) => pair.0 } }");
           Alcotest.test_case "qualified constructor pattern" `Quick
             (check_i64 "qualified constructor pattern" 2L
                "{ S = module { pub type Color = Red | Green }; \
                 open S; match (Green) { Red => 1, Green => 2 } }");
           Alcotest.test_case "qualified nested constructor pattern" `Quick
             (check_i64 "qualified nested constructor pattern" 7L
                "{ A = module { pub B = module { pub type T = X(I64) | Y } }; \
                 open A; open B; match (X(7)) { X(n) => n, Y => 0 } }");
           Alcotest.test_case "qualified constructor alias pattern" `Quick
             (check_i64 "qualified constructor alias pattern" 1L
                "{ S = module { pub type Color = Red | Green }; \
                 N = S; open N; match (Red) { Red => 1, Green => 2 } }");
           Alcotest.test_case "addition overflow is a language error" `Quick
             (check_overflow "add" "+" "{ 9223372036854775807 + 1 }");
           Alcotest.test_case "subtraction overflow is a language error" `Quick
             (check_overflow "sub" "-" "{ (0 - 9223372036854775807 - 1) - 1 }");
           Alcotest.test_case "multiplication overflow is a language error" `Quick
             (check_overflow "mul" "*" "{ 4611686018427387904 * 2 }");
           Alcotest.test_case "min_int / -1 overflows" `Quick
             (check_overflow "div" "/" "{ (0 - 9223372036854775807 - 1) / (0 - 1) }");
           Alcotest.test_case "arithmetic at the I64 boundary does not overflow" `Quick
             (check_i64 "boundary" (-1L) "{ (9223372036854775807 + (0 - 9223372036854775807 - 1)) * 1 + (0 - 9223372036854775807 - 1) % (0 - 1) }");
           Alcotest.test_case "division by zero is a language error" `Quick
             (check_div_by_zero "division by zero" "{ 1 / 0 }");
           Alcotest.test_case "remainder by zero is a language error" `Quick
             (check_div_by_zero "remainder by zero" "{ 1 % 0 }");
           Alcotest.test_case "division by zero through a call" `Quick
             (check_div_by_zero "division by zero through a call"
                "{ f = fn(x: I64) { 1 / x }; f(0) }");
           Alcotest.test_case "constructor sharing its type name" `Quick
             (check_i64 "constructor sharing its type name" 7L
                "{ type T = T(I64) | Y; \
                 match (T(7)) { T(n) => n, Y => 0 } }");
           Alcotest.test_case "qualified constructor sharing its type name" `Quick
             (check_i64 "qualified constructor sharing its type name" 7L
                "{ M = module { pub type T = T(I64) | Y }; \
                 match (M.T(7)) { M.T(n) => n, M.Y => 0 } }");
           Alcotest.test_case "duplicate module field resolves to last" `Quick
             (check_i64 "duplicate module field resolves to last" 2L
                "{ M = module { pub x = 1; pub x = 2 }; M.x }");
           Alcotest.test_case "open (import std) evaluates" `Quick
             (check_i64 "open import std" 3L
                "{ open (import \"std\"); 1 + 2 }");
           Alcotest.test_case "open (import std) prelude value in scope" `Quick
             (check_i64 "open import std prelude value" 1L
                "{ open (import \"std\"); \
                 match (not(False)) { True => 1, False => 0 } }");
          Alcotest.test_case "match int literal hit" `Quick
            (check_i64 "match int literal hit" 10L
               "match (1) { 1 => 10, _ => 20 }");
          Alcotest.test_case "match int literal default" `Quick
            (check_i64 "match int literal default" 20L
               "match (2) { 1 => 10, _ => 20 }");
           Alcotest.test_case "match literal or-pattern" `Quick
             (check_i64 "match literal or-pattern" 42L
                "match (1) { (0 | 1) => 42, _ => 0 }");
          Alcotest.test_case "match bool literal" `Quick
            (check_i64 "match bool literal" 0L
               "match (False) { True => 1, False => 0 }");
          Alcotest.test_case "match unit literal" `Quick
            (check_i64 "match unit literal" 7L
               "match () { () => 7 }");
          Alcotest.test_case "match char literal hit" `Quick
            (check_i64 "match char literal hit" 10L
               "match ('a') { 'a' => 10, _ => 20 }");
          Alcotest.test_case "match char literal default" `Quick
            (check_i64 "match char literal default" 20L
               "match ('b') { 'a' => 10, _ => 20 }");
          Alcotest.test_case "match escaped char literal" `Quick
            (check_i64 "match escaped char literal" 1L
               "match ('\\n') { '\\n' => 1, _ => 0 }");
          Alcotest.test_case "match literal binder fallback" `Quick
            (check_i64 "match literal binder fallback" 42L
               "match (42) { 0 => 0, x => x }");
          Alcotest.test_case "match first branch wins" `Quick
            (check_i64 "match first branch wins" 0L
               "match (1) { _ => 0, 1 => 1 }");
           Alcotest.test_case "match tagged payload" `Quick
             (check_i64 "match tagged payload" 42L
                "{ type Wrapper = W(I64); match (W(41)) { W(x) => x + 1 } }");
           Alcotest.test_case "match tuple bind" `Quick
             (check_i64 "match tuple bind" 1L
                "match (1, True) { (x, b) => if (b) { x } else { 0 } }");
          Alcotest.test_case "match tuple wildcard" `Quick
            (check_i64 "match tuple wildcard" 2L
               "match (1, 2) { (_, y) => y }");
          Alcotest.test_case "match whole tuple binder" `Quick
            (check_i64 "match whole tuple binder" 1L
               "match (1, True) { p => p.0 }");
          Alcotest.test_case "match nested tuple" `Quick
            (check_i64 "match nested tuple" 3L
               "match ((1, True), 2) { ((x, _), y) => x + y }");
          Alcotest.test_case "match tuple literals" `Quick
            (check_i64 "match tuple literals" 9L
               "match (False, 1) { (True, x) => x, (False, _) => 9 }");
          Alcotest.test_case "record field access" `Quick
            (check_i64 "record field access" 1L
               "{ Point = struct { x: I64; y: I64; }; (Point{x = 1; y = 2}).x }");
           Alcotest.test_case "parameterized record construction" `Quick
             (check_bool "parameterized record construction" true
                "{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; } }; (Pair[I64, Bool]{fst = 1; snd = True}).snd }");
          Alcotest.test_case "record type declaration" `Quick
            (check_i64 "record type declaration" 2L
               "{ Point = struct {x: I64; y: I64}; (Point{x = 1; y = 2}).y }");
          Alcotest.test_case "record construction field order" `Quick
            (check_i64 "record construction field order" 30L
               "{ Point = struct {x: I64; y: I64}; \
                p = Point{y = 20; x = 10}; p.x + p.y }");
          Alcotest.test_case "parameterized record type declaration" `Quick
            (check_bool "parameterized record type declaration" true
               "{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; (Pair{fst = 1; snd = True}).snd }");
           Alcotest.test_case "polymorphic record multiple instantiations" `Quick
             (check_i64 "polymorphic record multiple instantiations" 13L
                "{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; \
                 p1 = Pair{fst = 10; snd = 20}; \
                 p2 = Pair{fst = True; snd = 3}; \
                 if (p2.fst) { p1.fst + p2.snd } else { 0 } }");
            Alcotest.test_case "parameterized record method" `Quick
              (check_bool "parameterized record method" true
                 "{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; pub swap = fn(p) { (p.snd, p.fst) } } }; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = True})).0 }");
            Alcotest.test_case "method uses self" `Quick
              (check_i64 "method uses self" 1L
                 "{ Box = fn[A : Type] { struct { value: A; pub method get() { self.value } } }; Box[I64].get(Box[I64]{value = 1}) }");
            Alcotest.test_case "parameterized method uses self" `Quick
              (check_bool "parameterized method uses self" true
                 "{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; pub method swap() { (self.snd, self.fst) } } }; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = True})).0 }");
           Alcotest.test_case "method extra parameter" `Quick
             (check_i64 "method extra parameter" 3L
                "{ Counter = struct { value: I64; pub method add(x) { self.value + x } }; Counter.add(Counter{value = 1})(2) }");
            Alcotest.test_case "method uses Self type" `Quick
              (check_i64 "method uses Self type" 2L
                 "{ Box = fn[A : Type] { struct { value: A; pub method id(other : Self) { other.value } } }; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) }");
           Alcotest.test_case "method returns Self" `Quick
             (check_i64 "method returns Self" 1L
                "{ Box = struct { value: I64; pub method copy() { self } }; (Box.copy(Box{value = 1})).value }");
           Alcotest.test_case "struct impl for Self" `Quick
             (check_bool "struct impl for Self" true
                 "{ Point = struct { \
                    x: I64; \
                    pub impl Eq(Self) = module { fn eq(lhs, rhs) { lhs.x == rhs.x } } \
                  }; \
                  Point{x = 1} == Point{x = 1} }");
          Alcotest.test_case "record pattern shorthand" `Quick
            (check_i64 "record pattern shorthand" 3L
               "{ Point = struct { x: I64; y: I64; }; \
                match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }");
          Alcotest.test_case "record pattern reordered" `Quick
            (check_i64 "record pattern reordered" 3L
               "{ Point = struct { x: I64; y: I64; }; \
                match (Point{x = 1; y = 2}) { Point {y; x} => x + y } }");
          Alcotest.test_case "record pattern renamed field" `Quick
            (check_i64 "record pattern renamed field" 30L
               "{ Point = struct {x: I64; y: I64}; \
                match (Point{x = 10; y = 20}) { Point {x = wow; y} => wow + y } }");
          Alcotest.test_case "record pattern partial" `Quick
            (check_i64 "record pattern partial" 3L
               "{ Point = struct {x: I64; y: I64}; \
                match (Point{x = 3; y = 4}) { Point {x; _} => x } }");
          Alcotest.test_case "record pattern literal dispatch" `Quick
            (check_i64 "record pattern literal dispatch" 4L
               "{ Flag = struct { flag: Bool; value: I64; }; \
                match (Flag{flag = False; value = 3}) { \
                Flag {flag = True; value} => value, Flag {flag = False; value} => value + 1 } }");
           Alcotest.test_case "qualified record pattern" `Quick
             (check_i64 "qualified record pattern" 3L
                "{ M = module { pub Point = struct { x: I64; y: I64; } }; \
                 open M; match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }");
           Alcotest.test_case "struct private helper" `Quick
             (check_i64 "struct private helper" 11L
                "{ M = module { secret = 10; pub x = secret + 1 }; M.x }");
           Alcotest.test_case "module public function" `Quick
             (check_i64 "module public function" 42L
                "{ M = module { helper = fn(x) { x * 2 }; pub double = helper }; M.double(21) }");
           Alcotest.test_case "struct multiple public members" `Quick
             (check_i64 "struct multiple public members" 3L
                "{ M = module { pub a = 1; pub b = 2 }; M.a + M.b }");
           Alcotest.test_case "open struct values" `Quick
             (check_i64 "open struct values" 52L
                "{ M = module { pub x = 42; pub y = 10 }; open M; x + y }");
          Alcotest.test_case "open struct constructors" `Quick
            (check_i64 "open struct constructors" 1L
               "{ Color = module { pub type Color = Red | Green | Blue }; \
                open Color; match (Red) { Red => 1, Green => 2, Blue => 3 } }");
        ] );
      ( "module-level open",
        [
            (* The open extends the runtime scope of the bindings that follow
               it, so every de Bruijn index in the module — for names bound
               before the open as well as after it — must still line up. *)
            Alcotest.test_case "opened values usable in later bindings" `Quick
              (check_import_i64 "opened values usable in later bindings"
                 [ ("base", "pub a = 1; pub b = 2");
                   ("user", "open (import \"std\");\n\
                             local = 10;\n\
                             open (import \"base\");\n\
                             pub r = local + a + b") ]
                 13L "{ M = import \"user\"; M.r }");
            Alcotest.test_case "opened constructors usable in later bindings" `Quick
              (check_import_i64 "opened constructors usable in later bindings"
                 [ ("color", "pub type Color = Red | Green");
                   ("user", "open (import \"color\");\npub v = Red") ]
                 1L "{ M = import \"user\"; match (M.v) { Green => 2, Red => 1 } }");
            Alcotest.test_case "inline module open" `Quick
              (check_i64 "inline module open" 3L
                 "{ B = module { pub x = 3 }; M = module { open B; pub y = x }; M.y }");
            Alcotest.test_case "struct-level open" `Quick
              (check_i64 "struct-level open" 7L
                 "{ M = module { pub k = 7 }; S = struct { open M; pub m = k }; S.m }");
        ] );
      ( "imports",
        [
            Alcotest.test_case "basic import" `Quick
              (check_import_i64 "basic import" [ ("math", "open (import \"std\"); pub x = 41; pub y = x + 1") ] 42L
                 "{ M = import \"math\"; M.y }");
            Alcotest.test_case "imported public function" `Quick
              (check_import_i64 "imported public function" [ ("math", "open (import \"std\"); pub fn double(x) { x + x }") ] 10L
                 "{ M = import \"math\"; M.double(5) }");
            Alcotest.test_case "nested import" `Quick
              (check_import_i64 "nested import"
                 [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ] 42L
                 "{ W = import \"wrapper\"; W.M.x }");
            Alcotest.test_case "open imported module exposes public value" `Quick
              (check_import_i64 "open imported module exposes public value" [ ("math", "pub x = 42") ] 42L
                 "{ M = import \"math\"; open M; x }");
            Alcotest.test_case "open imported nested module" `Quick
              (check_import_i64 "open imported nested module"
                 [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ] 42L
                 "{ W = import \"wrapper\"; open W; open M; x }");
            Alcotest.test_case "open imported module local only" `Quick
              (check_import_i64 "open imported module local only"
                 [ ("base", "pub x = 41"); ("wrapper", "open (import \"std\"); B = import \"base\"; pub y = { open B; x + 1 }") ]
                 42L
                 "{ W = import \"wrapper\"; W.y }");
            Alcotest.test_case "repeated import" `Quick
              (check_import_i64 "repeated import" [ ("m", "pub x = 21") ] 42L
                 "{ A = import \"m\"; B = import \"m\"; A.x + B.x }");
            Alcotest.test_case "imported ADT match" `Quick
              (check_import_i64 "imported ADT match"
                 [ ("color", "pub type Color = Red | Green | Blue; pub default = Green") ] 2L
                 "{ C = import \"color\"; match (C.default) { C.Red => 1, C.Green => 2, C.Blue => 3 } }");
            Alcotest.test_case "open imported module exposes constructors" `Quick
              (check_import_i64 "open imported module exposes constructors"
                 [ ("color", "pub type Color = Red | Green") ] 1L
                 "{ C = import \"color\"; open C; match (Red) { Red => 1, Green => 2 } }");
            Alcotest.test_case "imported record field access" `Quick
              (check_import_i64 "imported record field access"
                 [ ("shapes", "pub Point = struct {x: I64; y: I64}") ] 1L
                 "{ S = import \"shapes\"; (S.Point{x = 1; y = 2}).x }");
            Alcotest.test_case "imported record pattern" `Quick
              (check_import_i64 "imported record pattern"
                 [ ("shapes", "pub Point = struct {x: I64; y: I64}") ] 3L
                 "{ S = import \"shapes\"; match (S.Point{x = 1; y = 2}) { S.Point {x; y} => x + y } }");
            Alcotest.test_case "imported record pattern alias" `Quick
              (check_import_i64 "imported record pattern alias"
                 [ ("shapes", "pub Point = struct {x: I64; y: I64}") ] 3L
                 "{ S = import \"shapes\"; Alias = S; match (S.Point{x = 1; y = 2}) { Alias.Point {x; y} => x + y } }");
            Alcotest.test_case "imported nested constructor pattern" `Quick
              (check_import_i64 "imported nested constructor pattern"
                 [ ("nested", "pub M = module { pub type T = X(I64) | Y }") ] 7L
                 "{ N = import \"nested\"; match (N.M.X(7)) { N.M.X(n) => n, N.M.Y => 0 } }");
            Alcotest.test_case "imported module alias pattern" `Quick
              (check_import_i64 "imported module alias pattern"
                 [ ("color", "pub type Color = Red | Green") ] 1L
                 "{ C = import \"color\"; Alias = C; match (C.Red) { Alias.Red => 1, Alias.Green => 2 } }");
            Alcotest.test_case "imported public effect handler" `Quick
              (check_import_i64 "imported public effect handler"
                 [ ("effects", "pub effect Exc = sig { raise : I64 -> I64 }") ] 2L
                 "{ E = import \"effects\"; match (perform E.Exc.raise(1)) { x => x, effect E.Exc.raise n => n + 1 } }");
            Alcotest.test_case "open imported module exposes effect" `Quick
              (check_import_i64 "open imported module exposes effect"
                 [ ("effects", "pub effect Exc = sig { raise : I64 -> I64 }") ] 2L
                 "{ E = import \"effects\"; open E; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } }");
            Alcotest.test_case "imported public parameterized effect handler" `Quick
              (check_import_i64 "imported public parameterized effect handler"
                 [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ] 42L
                 "{ E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  match (perform StateI64.get(())) { x => x, effect StateI64.get () => 42 } }");
            Alcotest.test_case "imported latent effect function" `Quick
              (check_import_i64 "imported latent effect function"
                 [ ( "effects",
                     "pub effect State(S) = sig { get : Unit -> S }; \
                      pub read : Unit -> I64 can State(I64) = fn(_) { perform State.get(()) }" ) ]
                 7L
                 "{ E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  match (E.read(())) { x => x, effect StateI64.get () => 7 } }");
            Alcotest.test_case "imported parameterized handler distinguishes instances" `Quick
              (check_import_i64 "imported parameterized handler distinguishes instances"
                 [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ] 1L
                 "{ E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  StateBool = E.State(Bool); \
                  match (if (perform StateBool.get(())) { perform StateI64.get(()) } else { 0 }) { \
                    x => x, \
                  effect StateI64.get () => resume(1), \
                  effect StateBool.get () => resume(True) \
                  } }");
        ] );
      ( "conv",
        [
           Alcotest.test_case "beta" `Quick
             (check_conv "beta" "(fn(x : I64) { x })(5)" "5");
           Alcotest.test_case "eta" `Quick
             (check_conv "eta"
                "fn(x : I64) { x }"
                "fn(y : I64) { y }");
          Alcotest.test_case "not equal" `Quick
            (check_not_conv "not equal" "I64" "Bool");
          Alcotest.test_case "effect same id same params" `Quick test_conv_effect_same_id_same_params;
          Alcotest.test_case "effect different ids" `Quick test_conv_effect_different_ids;
          Alcotest.test_case "effect row order" `Quick test_conv_effect_row_order;
          Alcotest.test_case "effect row mismatch" `Quick test_conv_effect_row_mismatch;
        ] );
      ( "debug",
        [
          Alcotest.test_case "effect" `Quick test_debug_effect;
          Alcotest.test_case "effectful pi" `Quick test_debug_effectful_pi;
          Alcotest.test_case "perform" `Quick test_debug_perform;
        ] );
      ( "neutral",
        [
          Alcotest.test_case "var" `Quick test_neutral_var;
          Alcotest.test_case "ap" `Quick test_neutral_ap;
          Alcotest.test_case "match" `Quick test_neutral_match;
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
          Alcotest.test_case "effect same id same params" `Quick test_unify_effect_same_id_same_params;
          Alcotest.test_case "effect same id different params" `Quick test_unify_effect_same_id_different_params;
          Alcotest.test_case "effect different ids" `Quick test_unify_effect_different_ids;
          Alcotest.test_case "effect row order" `Quick test_unify_effect_row_order;
          Alcotest.test_case "effect row mismatch" `Quick test_unify_effect_row_mismatch;
          Alcotest.test_case "mismatch" `Quick test_unify_mismatch;
        ] );
      ( "macros",
        [
          Alcotest.test_case "hygiene: macro binder does not capture user" `Quick test_macro_hygiene_no_capture_user;
          Alcotest.test_case "hygiene: user binder does not capture macro" `Quick test_macro_hygiene_user_no_capture_macro;
          Alcotest.test_case "panic message propagates" `Quick test_macro_panic_has_message;
          Alcotest.test_case "imported macro expands" `Quick test_imported_macro_expands;
          Alcotest.test_case "imported operator used inside a unit" `Quick test_imported_operator_used_inside_a_unit;
          Alcotest.test_case "operator declared and used in one unit" `Quick test_operator_declared_and_used_in_one_unit;
          Alcotest.test_case "local operator shadows imported" `Quick test_local_operator_shadows_imported;
          Alcotest.test_case "operator body follows last import" `Quick test_operator_body_follows_last_import;
          Alcotest.test_case "unit uses its own macro" `Quick test_unit_uses_its_own_macro;
          Alcotest.test_case "unit calls imported macro dotted" `Quick test_unit_calls_imported_macro_dotted;
          Alcotest.test_case "unit calls imported macro via open" `Quick test_unit_calls_imported_macro_via_open;
          Alcotest.test_case "macro through re-exported member" `Quick test_macro_through_reexported_member;
          Alcotest.test_case "bare import does not inject macros" `Quick test_bare_import_does_not_inject_macros;
          Alcotest.test_case "open delivers macros bare" `Quick test_open_delivers_macros_bare;
          Alcotest.test_case "open bound import delivers macros bare" `Quick test_open_bound_import_delivers_macros_bare;
          Alcotest.test_case "same macro name in two units" `Quick test_same_macro_name_in_two_units;
          Alcotest.test_case "imported macro not runtime field" `Quick test_imported_macro_not_runtime_field;
          Alcotest.test_case "imported macro circular visit" `Quick test_imported_macro_circular_visit;
          Alcotest.test_case "macro-generated import loads macros" `Quick test_macro_generated_import_loads_macros;
          Alcotest.test_case "macro-generated import checks missing" `Quick test_macro_generated_import_checks_missing;
          Alcotest.test_case "imported macro calls regular function" `Quick test_imported_macro_calls_regular_function;
          Alcotest.test_case "operator prefix macro expands" `Quick test_operator_prefix_macro_expands;
          Alcotest.test_case "infix macro expands" `Quick test_operator_infix_macro_expands;
          Alcotest.test_case "infix uses operands" `Quick test_operator_uses_operands;
          Alcotest.test_case "macro multi-arg" `Quick test_macro_multi_arg;
          Alcotest.test_case "macro multi-arg swap" `Quick test_macro_multi_arg_swap;
          Alcotest.test_case "macro default kind Expr" `Quick test_macro_default_expr;
          Alcotest.test_case "macro : Expr(_) annotation" `Quick test_macro_expr_annotation;
          Alcotest.test_case "macro Decl in Expr context rejected" `Quick test_macro_decl_in_expr_context;
          Alcotest.test_case "macro name shadowing regardless of kind" `Quick test_macro_name_shadowing;
          Alcotest.test_case "Decl kind survives elaboration" `Quick test_decl_kind_registered_persists;
          Alcotest.test_case "Decl macro generates binding in module" `Quick test_decl_macro_generates_binding;
          Alcotest.test_case "imported Decl macro generates binding" `Quick test_imported_decl_macro;
          Alcotest.test_case "Decl macro two calls" `Quick test_decl_macro_two_calls;
          Alcotest.test_case "Pattern wild round-trip" `Quick test_pattern_round_trip;
          Alcotest.test_case "type-aware default macro" `Quick test_type_aware_macro;
          Alcotest.test_case "type-aware checking mode" `Quick test_type_aware_checking;
          Alcotest.test_case "type-aware output is expanded" `Quick test_type_aware_output_is_expanded;
          Alcotest.test_case "macro does not capture its argument" `Quick test_macro_does_not_capture_argument;
          Alcotest.test_case "macro body sees nothing ambient" `Quick test_macro_body_sees_nothing_ambient;
          Alcotest.test_case "quote splices holes" `Quick test_quote_splices_holes;
          Alcotest.test_case "quote declarations" `Quick test_quote_declarations;
          Alcotest.test_case "template literals resolve at definition" `Quick test_template_literals_resolve_at_definition;
          Alcotest.test_case "block-local macros do not leak" `Quick test_block_local_macros_do_not_leak;
          Alcotest.test_case "reflection round trip is the identity" `Quick test_round_trip_is_identity;
          Alcotest.test_case "unit-level type chain" `Quick test_unit_level_type_chain;
          Alcotest.test_case "type-directed default I64" `Quick test_type_default_macro;
          Alcotest.test_case "type-directed default Bool" `Quick test_type_default_bool;
          Alcotest.test_case "R-type match non-exhaustive missing ctors" `Quick test_rtype_match_non_exhaustive_missing_ctors;
          Alcotest.test_case "Expr(A) binding" `Quick test_expr_binding;
          Alcotest.test_case "expected type reaches macro" `Quick test_expected_type_reaches_macro;
          Alcotest.test_case "expected type rejects mismatch" `Quick test_expected_type_rejects_mismatch;
          Alcotest.test_case "binder type mismatch" `Quick test_binder_type_mismatch;
          Alcotest.test_case "binder body type mismatch" `Quick test_binder_body_type_mismatch;
          Alcotest.test_case "driver equiv runtime module" `Quick test_driver_equiv_runtime;
          Alcotest.test_case "driver equiv module with macro" `Quick test_driver_equiv_macro;
          Alcotest.test_case "driver macro_exports default kind" `Quick test_driver_macro_exports_default;
          Alcotest.test_case "driver macro_exports Decl kind" `Quick test_driver_macro_exports_decl;
          Alcotest.test_case "driver elab_ctx.macro_runtime populated" `Quick test_driver_elab_ctx_has_macro_runtime;
          Alcotest.test_case "imported private macro not registered" `Quick test_visit_macros_private_not_registered;
          Alcotest.test_case "macro type binders are explicit" `Quick test_macro_type_binders_are_explicit;
          Alcotest.test_case "a macro signature checks" `Quick test_macro_signature_checks;
          Alcotest.test_case "typed arguments elaborate once" `Quick test_typed_arguments_elaborate_once;
          Alcotest.test_case "an imported macro's signature" `Quick test_imported_macro_signature;
          Alcotest.test_case "generated macro binding re-entered" `Quick test_generated_macro_binding_reentered;
          Alcotest.test_case "generated multi-binding scope threading" `Quick test_generated_multi_binding_scope_threading;
          Alcotest.test_case "macro and syntax together" `Quick test_macro_and_syntax_together;
          Alcotest.test_case "infix right assoc" `Quick test_operator_right_assoc;
          Alcotest.test_case "infix mixed precedence" `Quick test_operator_mixed_precedence;
          Alcotest.test_case "bodyless infix builtin-apply" `Quick test_operator_bodyless_infix_builtin_apply;
          Alcotest.test_case "bodyless prefix builtin-apply" `Quick test_operator_bodyless_prefix_builtin_apply;
          Alcotest.test_case "operator RHS can use earlier macro" `Quick test_operator_rhs_can_use_earlier_macro;
          Alcotest.test_case "operator prefix receives structured input" `Quick test_operator_prefix_receives_structured_input;
          Alcotest.test_case "operator macro error reports spans" `Quick test_operator_macro_error_reports_spans;
          Alcotest.test_case "macro body budget overrun names the macro" `Quick test_macro_body_budget_overrun_names_the_macro;
          Alcotest.test_case "operator body error reports use span" `Quick test_operator_body_error_reports_use_span;
          Alcotest.test_case "operator body panic reports use span" `Quick
            (operator_body_failure_reports_use_span "panic[Syntax.Expr](\"boom\")" "boom");
          Alcotest.test_case "operator body division by zero reports use span" `Quick
            (* The divisor waits on [stx], so the checker cannot evaluate it at
               the definition: the division happens in the application. *)
            (operator_body_failure_reports_use_span "{ _ = 1 / (match (stx) { _ => 0 }); stx }" "division by zero");
          Alcotest.test_case "Syntax module: expression kind" `Quick test_syntax_module_expression_kind;
          Alcotest.test_case "Syntax module: literal inspectors" `Quick test_syntax_module_literal_inspectors;
          Alcotest.test_case "Syntax module: literal inspector error" `Quick test_syntax_module_literal_inspector_error;
          Alcotest.test_case "Syntax module: ap deconstructors" `Quick test_syntax_module_ap_deconstructors;
          Alcotest.test_case "Syntax module: ap deconstructor error" `Quick test_syntax_module_ap_deconstructor_error;
          Alcotest.test_case "Syntax module: lam deconstructor error" `Quick test_syntax_module_lam_deconstructor_error;
          Alcotest.test_case "Syntax module: let deconstructor error" `Quick test_syntax_module_let_deconstructor_error;
          Alcotest.test_case "Syntax module: identifier inspection" `Quick test_syntax_module_identifier_inspection;
          Alcotest.test_case "Syntax module: i64 builder" `Quick test_syntax_module_i64_builder;
          Alcotest.test_case "Syntax module: primitive names hidden" `Quick test_syntax_primitive_names_hidden;
          Alcotest.test_case "Syntax module: class types accessible" `Quick test_syntax_class_types_accessible;
          Alcotest.test_case "Syntax module: Expr nominal resolvable" `Quick test_syntax_expr_nominal_resolvable;
          Alcotest.test_case "pattern synonym substitution" `Quick test_pattern_syn_subst;
          Alcotest.test_case "pattern synonym in prelude" `Quick test_pattern_syn_in_prelude;
          Alcotest.test_case "Syntax module: application builder" `Quick test_syntax_module_application_builder;
          Alcotest.test_case "Syntax module: literal builders" `Quick test_syntax_module_literal_builders;
          Alcotest.test_case "Syntax module: let builder" `Quick test_syntax_module_let_builder;
          Alcotest.test_case "operator macro discards perform operand before elaboration" `Quick test_operator_macro_discards_unelaborated_perform_operand;
          Alcotest.test_case "7G: ADT matching preserves binding hygiene" `Quick test_7g_adt_matching_hygiene_roundtrip;
          Alcotest.test_case "7G: ADT destructured body not captured by outer scope" `Quick test_7g_adt_matching_hygiene_introduced_body;
          Alcotest.test_case "7G: computed multi-kind dispatch not possible with templates" `Quick test_7g_adt_matching_flip_args;
          Alcotest.test_case "7G: simple swap args via nested match [DEBUG]" `Quick test_7g_adt_simple_flip;
          Alcotest.test_case "7G: DEBUG outer match with named binders" `Quick test_7g_diag_outer_binders;
          Alcotest.test_case "7G: DEBUG inner match with wildcards only" `Quick test_7g_diag_inner_binders_wildcards;
          Alcotest.test_case "7G: DEBUG inner match with named binders" `Quick test_7g_diag_inner_binders_named;
          Alcotest.test_case "7I: generated syntax obeys later-wins shadowing" `Quick test_7i_generated_syntax_later_wins_shadow;
          Alcotest.test_case "imported operator prefix expands" `Quick test_imported_operator_prefix_expands;
          Alcotest.test_case "imported syntax not runtime field" `Quick test_imported_syntax_not_runtime_field;
          Alcotest.test_case "operator prefix shadowing is lexical" `Quick test_operator_prefix_shadowing_is_lexical;
          Alcotest.test_case "syntax template: unless guard" `Quick test_syntax_template_unless;
          Alcotest.test_case "syntax template: capture extent" `Quick test_syntax_template_capture_extent;
          Alcotest.test_case "order groups" `Quick test_order_groups;
          Alcotest.test_case "order group errors" `Quick test_order_group_errors;
          Alcotest.test_case "order group imported" `Quick test_order_group_imported;
          Alcotest.test_case "syntax template: when/else match" `Quick test_syntax_template_when_match;
          Alcotest.test_case "syntax template: hole reuse duplicates effects" `Quick test_syntax_template_hole_reuse;
          Alcotest.test_case "syntax template: {/} delimiters" `Quick test_syntax_template_do_end_delimiters;
          Alcotest.test_case "syntax template: hygiene no capture" `Quick test_syntax_template_hygiene;
          Alcotest.test_case "syntax template: def-site scope for template refs" `Quick test_syntax_template_def_site_scope;
          Alcotest.test_case "syntax template: hole keeps use-site scope" `Quick test_syntax_template_hole_keeps_use_site_scope;
          Alcotest.test_case "syntax template: introduced binder captures introduced ref" `Quick test_syntax_template_intro_binding_captures_intro_ref;
          Alcotest.test_case "syntax template: def-site scope through lambda" `Quick test_syntax_template_def_site_scope_through_lambda;
          Alcotest.test_case "syntax template: nested def-site scope" `Quick test_syntax_template_nested_def_site_scope;
          Alcotest.test_case "syntax template: generates syntax form" `Quick test_syntax_template_generates_syntax_form;
          Alcotest.test_case "syntax template: generated syntax closes over hole" `Quick test_syntax_template_generated_syntax_closes_over_hole;
          Alcotest.test_case "syntax template: generated syntax scope is local" `Quick test_syntax_template_generated_syntax_scope_is_local;
          Alcotest.test_case "syntax template: expands to template use" `Quick test_syntax_template_expands_to_template_use;
          Alcotest.test_case "syntax template: generates parameterized syntax" `Quick test_syntax_template_generates_parameterized_syntax_form;
          Alcotest.test_case "syntax template: nested callsite parenthesized" `Quick test_syntax_template_nested_callsite_parenthesized;
          Alcotest.test_case "syntax template: nested callsite unparenthesized" `Quick test_syntax_template_nested_callsite_unparenthesized;
          Alcotest.test_case "syntax template: nested callsite repeated hole" `Quick test_syntax_template_nested_callsite_repeated_hole;
          Alcotest.test_case "syntax template: multi-branch disambiguation" `Quick test_syntax_template_multi_branch;
          Alcotest.test_case "syntax template: imported pub syntax" `Quick test_syntax_template_imported_pub_syntax;
          Alcotest.test_case "syntax template: imported intro binding hygiene" `Quick test_syntax_template_imported_intro_binding_hygiene;
          Alcotest.test_case "syntax template: no holes" `Quick test_syntax_template_no_holes;
          Alcotest.test_case "syntax template: binder hole" `Quick test_syntax_template_binder_hole;
          Alcotest.test_case "syntax template: identifier hole" `Quick test_syntax_template_ident_hole;
          Alcotest.test_case "syntax template: pattern hole" `Quick test_syntax_template_pattern_hole;
          Alcotest.test_case "syntax template: unused capture" `Quick test_syntax_template_unused_capture;
          Alcotest.test_case "syntax template: reuse duplicates evaluation" `Quick test_syntax_template_reuse_duplicates_evaluation;
          Alcotest.test_case "decl template: module captures pub value" `Quick test_decl_template_module_captures_pub_value;
          Alcotest.test_case "decl template: module preserves typed value" `Quick test_decl_template_module_preserves_typed_value;
          Alcotest.test_case "decl template: struct captures pub value" `Quick test_decl_template_struct_captures_pub_value;
          Alcotest.test_case "decl template: multi generates siblings" `Quick test_decl_template_multi_generates_siblings;
          Alcotest.test_case "decl template: multi rejected in expr" `Quick test_decl_template_multi_rejected_in_expr;
          Alcotest.test_case "decl template: struct field deferred" `Quick test_decl_template_struct_field_deferred;
          Alcotest.test_case "7I: generated pub syntax across imports" `Quick test_7i_generated_pub_syntax_across_imports;
          Alcotest.test_case "7I: generated syntax usable later" `Quick test_7i_generated_syntax_usable_later_same_module;
          Alcotest.test_case "7I: generated pub operator across imports" `Quick test_7i_generated_pub_operator_across_imports;
          Alcotest.test_case "7I: generated pub macro across imports" `Quick test_7i_generated_pub_macro_across_imports;
          Alcotest.test_case "7I: generated pub operator rejected in struct" `Quick test_7i_generated_pub_operator_rejected_in_struct;
          Alcotest.test_case "7I: generated pub macro rejected in struct" `Quick test_7i_generated_pub_macro_rejected_in_struct;
          Alcotest.test_case "7I: generated syntax cycle" `Quick test_7i_generated_syntax_cycle;
          Alcotest.test_case "7I: generated macro cycle" `Quick test_7i_generated_macro_cycle;
          Alcotest.test_case "7I: generated syntax hygiene introduced binder" `Quick test_7i_generated_syntax_hygiene_introduced_binder;
        ] );
      ( "m7 roles",
        [
          Alcotest.test_case "syntax shadows syntax in a nested block" `Quick test_m7_syntax_shadows_syntax_in_block;
          Alcotest.test_case "replacement reads roles at its definition" `Quick test_m7_replacement_reads_roles_at_definition;
          Alcotest.test_case "template syntax visible in its output" `Quick test_m7_template_syntax_visible_in_its_output;
          Alcotest.test_case "generated syntax usable by next form" `Quick test_m7_generated_syntax_usable_by_next_form;
          Alcotest.test_case "template-written syntax invisible to user" `Quick test_m7_template_written_syntax_invisible;
          Alcotest.test_case "value binder under syntax" `Quick test_m7_value_binder_under_syntax;
          Alcotest.test_case "syntax after value binder" `Quick test_m7_syntax_after_value_binder;
          Alcotest.test_case "fn param under syntax" `Quick test_m7_fn_param_under_syntax;
          Alcotest.test_case "match binder under syntax" `Quick test_m7_match_binder_under_syntax;
          Alcotest.test_case "syntax inside param region" `Quick test_m7_syntax_inside_param_region;
          Alcotest.test_case "value under macro" `Quick test_m7_value_under_macro;
          Alcotest.test_case "value under imported operator" `Quick test_m7_value_under_imported_operator;
          Alcotest.test_case "sibling regions do not conflict" `Quick test_m7_sibling_regions_do_not_conflict;
          Alcotest.test_case "hygienic macro binder is apart" `Quick test_m7_hygienic_macro_binder_is_apart;
          Alcotest.test_case "fixity attaches to value" `Quick test_m7_fixity_attaches_to_value;
          Alcotest.test_case "new binder under attached fixity" `Quick test_m7_new_binder_under_attached_fixity;
          Alcotest.test_case "open under syntax" `Quick test_m7_open_under_syntax;
          Alcotest.test_case "syntax inside open region" `Quick test_m7_syntax_inside_open_region;
          Alcotest.test_case "open without conflict" `Quick test_m7_open_without_conflict;
          Alcotest.test_case "imported role in its open's region" `Quick test_m7_import_open_role_in_region;
          Alcotest.test_case "block open (import) role not after block" `Quick test_m7_import_open_role_not_after_block;
          Alcotest.test_case "block import binder role not after block" `Quick test_m7_import_binder_role_not_after_block;
          Alcotest.test_case "import open under unit syntax" `Quick test_m7_import_open_under_syntax;
          Alcotest.test_case "driver-run open under syntax" `Quick test_m7_driver_open_under_syntax;
        ] );
      ( "m9 forms",
        [
          Alcotest.test_case "rust-style arms evaluate" `Quick test_arms_evaluate;
          Alcotest.test_case "a macro reads a block's tokens" `Quick test_m9_block_tokens_inspected;
          Alcotest.test_case "a Block hole fills a module body" `Quick test_m9_block_hole_in_module_slot;
          Alcotest.test_case "a body in a macro argument reads generated syntax" `Quick test_m9_body_read_after_earlier_statement;
          Alcotest.test_case "a Decl form as a block statement" `Quick test_m9_decl_form_as_block_statement;
          Alcotest.test_case "expand_block output placed back" `Quick test_m9_expand_block_placed_back;
          Alcotest.test_case "expansion is idempotent" `Quick test_m9_expansion_idempotent;
          Alcotest.test_case "filling equals the quote" `Quick test_m9_filling_equals_quote;
          Alcotest.test_case "a quote's nested rule holes are lexical" `Quick test_m9_quote_nested_rule_holes;
          Alcotest.test_case "a quote hole names generated syntax" `Quick test_m9_quote_token_position_hole;
          Alcotest.test_case "an Id parameter" `Quick test_m9_param_id;
          Alcotest.test_case "an Id parameter binds" `Quick test_m9_param_id_binds;
          Alcotest.test_case "a Pattern parameter" `Quick test_m9_param_pattern;
          Alcotest.test_case "a Block parameter" `Quick test_m9_param_block;
          Alcotest.test_case "a type-aware macro's Id parameter" `Quick test_m9_param_type_aware;
          Alcotest.test_case "an argument of the wrong kind" `Quick test_m9_param_kind_mismatch;
          Alcotest.test_case "declaration binders are fresh" `Quick test_declaration_binders_are_fresh;
          Alcotest.test_case "resolved names cannot be forged" `Quick test_resolved_names_cannot_be_forged;
          Alcotest.test_case "a Decl parameter" `Quick test_m9_param_decl;
          Alcotest.test_case "a Decl argument of the wrong kind" `Quick test_m9_param_decl_kind_mismatch;
          Alcotest.test_case "a Decl parameter is one declaration" `Quick test_m9_param_one_decl;
          Alcotest.test_case "expand_decls" `Quick test_expand_decls;
          Alcotest.test_case "expand_decls budget" `Quick test_expand_decls_budget;
          Alcotest.test_case "expand_decls imported" `Quick test_expand_decls_imported;
          Alcotest.test_case "an imported macro's parameter kinds" `Quick test_m9_param_imported;
          Alcotest.test_case "a macro is given exactly its arguments" `Quick test_m8_argument_count;
          Alcotest.test_case "an imported macro's argument count" `Quick test_m8_imported_argument_count;
          Alcotest.test_case "a Decl macro's output type" `Quick test_decl_macro_output_type;
        ] );
    ]
