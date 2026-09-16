open Core
open Atom
open Nbe
open Unify

let mc () = MetaContext.create ()
let pure_effects = effect_row_closure [] empty_effect_row

(* Prelude syntax exports (operators, [if]) — passed into every parse so test
   sources can use [+]/[==]/[if]/… now that these are prelude features rather
   than compiler builtins. Mirrors what the entry points and loader inject. *)
let builtin_syntax = Macro_driver.std_syntax ()

let parse_expr source =
  Parse_expand.parse_expr ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source

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

let check_i64 label expected source () =
  match eval_source source with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      fail_with_source label source
        (Printf.sprintf "expected VAtom I64, got %s" (Debug.pp_value_short mc v))
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

let test_eval_module_is_not_a_signature () =
  match eval_source "{ Types = module { pub x = I64 }; f = fn(m : Types) { m.x }; f(module { pub x = 42 }) }" with
  | exception Elab_error.ElabError (Elab_error.NotASignature (Some "Types")) -> ()
  | exception e -> Alcotest.fail ("module as a type: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "module as a type: expected NotASignature"

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
  let nom1 = VNominal { id = 99; num_params = 1; name = "Option"; captures = []; params = [ VAtomTy Atom_ty.TI64 ] } in
  let nom2 = VNominal { id = 99; num_params = 1; name = "Option"; captures = []; params = [ VAtomTy Atom_ty.TChar ] } in
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
      effects = { env = effect_row_env (); effects; tails = [] };
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
  if not (String.contains text '{') then Alcotest.fail ("expected an arrow row in debug output, got " ^ text)

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
  (* A pure result reports the same effects under its own name. *)
  | exception Elaborate.ElabError ((UnhandledEffects got | EffectsInPureResult got)) when got = names -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an unhandled-effect error")

let test_top_unhandled_perform () =
  expect_unhandled "a top-level perform" [ "effect Exc" ] (fun () ->
      eval_source "{ effect Exc = sig { raise : I64 -> I64 }; perform Exc.raise(1) }")

let test_top_escaping_closure () =
  (* The other branch's pure closure fixes the result row; the escape is still named (E6). *)
  match
    eval_source
      "{ effect Exc = sig { raise : I64 -> I64 };
         g = match (0) { x => fn(u : Unit) { perform Exc.raise(x) }, effect Exc.raise n => fn(u : Unit) { n } };
         g(()) }"
  with
  | exception Elaborate.ElabError (HandledEffectEscapes "Exc") -> ()
  | exception e -> Alcotest.fail (Printexc.to_string e)
  | _ -> Alcotest.fail "expected HandledEffectEscapes"

let test_handled_effect_escape () =
  (match eval_source "{ effect Exc = sig { raise : I64 -> I64 }; g = match (0) { x => fn(u : Unit) { perform Exc.raise(x) }, effect Exc.raise n => fn(u : Unit) { perform Exc.raise(n) } }; 1 }" with
   | exception Elaborate.ElabError (HandledEffectEscapes "Exc") -> ()
   | exception e -> Alcotest.fail (Printexc.to_string e)
   | _ -> Alcotest.fail "expected HandledEffectEscapes");
  (match eval_source "{ effect Exc = sig { raise : I64 -> I64 }; q : Ref(Unit ->{Exc} I64) = ref(fn(u) { perform Exc.raise(0) }); _ = match (0) { x => { q <- fn(u : Unit) { perform Exc.raise(x) }; 1 }, effect Exc.raise n => 2 }; 5 }" with
   | exception Elaborate.ElabError (HandledEffectEscapes "Exc") -> ()
   | exception e -> Alcotest.fail (Printexc.to_string e)
   | _ -> Alcotest.fail "expected HandledEffectEscapes through an outer ref");
  (match eval_source "{ effect Exc = sig { raise : I64 -> I64 }; g = match (0) { x => module { pub f = fn(u : Unit) { perform Exc.raise(x) } }, effect Exc.raise n => module { pub f = fn(u : Unit) { perform Exc.raise(n) } } }; 1 }" with
   | exception Elaborate.ElabError (HandledEffectEscapes "Exc") -> ()
   | exception e -> Alcotest.fail (Printexc.to_string e)
   | _ -> Alcotest.fail "expected HandledEffectEscapes through a module member");
  check_i64 "a local ref may hold a handled closure" 1L
    "{ effect Exc = sig { raise : I64 -> I64 }; match (0) { x => { q = ref(fn(u : Unit) { perform Exc.raise(x) }); 1 }, effect Exc.raise n => 2 } }" ();
  check_i64 "a saved continuation may outlive its handler" 5L
    "{ effect Async = sig { pause : Unit -> Unit }; q = ref(fn(u : Unit) { 0 }); _ = match (perform Async.pause(())) { v => 1, effect Async.pause _ => { q <- fn(u : Unit) { resume(()) }; 2 } }; 5 }" ()

let test_top_unhandled_in_imported_unit () =
  with_modules [ ("noisy", "effect Exc = sig { raise : I64 -> I64 };\npub v = perform Exc.raise(1)") ] (fun loader ->
      expect_unhandled "an imported unit's top-level perform" [ "effect Exc" ] (fun () ->
          eval_source_with_loader loader "{ M = import \"noisy\"; 0 }"))

(* A method follows the arrow rule: pure unless its [->{E} T] declares a row. *)
let exc_counter methods = "effect Exc = sig { raise : I64 -> I64 }; C = struct { n : I64; " ^ methods ^ " }"

let test_method_rows () =
  expect_unhandled "a method performing an undeclared effect" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method bump() { perform Exc.raise(1); self.n }" ^ "; 0 }"));
  check_i64 "a method declaring its row, handled at the call" 11L
    ("{ " ^ exc_counter "pub method bump() ->{Exc} I64 { perform Exc.raise(1); self.n }"
     ^ "; match (C.bump(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }") ();
  expect_unhandled "a declared method called at the top without a handler" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method bump() ->{Exc} I64 { perform Exc.raise(1); self.n }" ^ "; C.bump(C{n = 1}) }"));
  check_i64 "->{_} infers a method's row" 13L
    ("{ " ^ exc_counter "pub method add(k : I64) ->{_} I64 { perform Exc.raise(k) }"
     ^ "; match (C.add(C{n = 1})(3)) { x => x, effect Exc.raise v => v + 10 } }") ();
  check_i64 "a method calling another performs its declared row" 12L
    ("{ " ^ exc_counter "pub method a(k : I64) ->{Exc} I64 { perform Exc.raise(k) }; pub method b() ->{Exc} I64 { a(self)(2) }"
     ^ "; match (C.b(C{n = 1})) { x => x, effect Exc.raise v => v + 10 } }") ();
  expect_unhandled "a pure method calling an effectful one" [ "effect Exc" ] (fun () ->
      eval_source ("{ " ^ exc_counter "pub method a(k : I64) ->{Exc} I64 { perform Exc.raise(k) }; pub method b() : I64 { a(self)(2) }" ^ "; 0 }"))

let test_trait_method_rows () =
  let trait_src impl_body =
    "{ effect Exc = sig { raise : I64 -> I64 }; effect Other = sig { ping : I64 -> I64 }; \
     trait Log(A) = sig { log : A ->{Exc} I64 }; \
     impl Log(I64) = module { log = fn(x) { " ^ impl_body ^ " } }; 0 }"
  in
  check_i64 "an impl method within its trait's row" 0L (trait_src "perform Exc.raise(x)") ();
  expect_unhandled "an impl method performing beyond its trait's row" [ "effect Other" ] (fun () ->
      eval_source (trait_src "perform Other.ping(x)"))

let test_debug_perform () =
  let text = Debug.pp_term (Perform { eff = EffectRef { id = 0; name = "State"; params = [ AtomTy Atom_ty.TI64 ] }; op = "get"; arg = Atom Unit }) in
  if not (String.contains text 'g') then Alcotest.fail ("expected perform debug output, got " ^ text)

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

let test_eval_type_case_default_string_panics () =
  match eval_source (default_source "default[String]") with
  | exception Nbe.EvalError "no default" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected panic"

let test_eval_equality_nominal_rejected () =
  match eval_source "{ type Color = Red; Red == Red }" with
  (* [Eq] is in scope; what is missing is an impl. The error used to say
     [UnknownTrait]. See the impl-visibility topic. *)
  | exception Elaborate.ElabError (MissingTraitImplementation _) -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected missing Eq impl"

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
  let expr, expand_ctx = Parse_expand.parse_expr_with_ctx ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source in
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
  let expr, expand_ctx = Parse_expand.parse_module_with_ctx ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~load_syntax:Macro_driver.std_load_syntax source in
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

let test_export_unit_macros () =
  macro_in_unit "a re-exported macro, dotted" 42L
    [ answers_42; ("re", "I = import \"inner\";\nexport I") ]
    "{ R = import \"re\"; R.answer(0) }";
  macro_in_unit "a re-exported macro, opened" 42L
    [ answers_42; ("re", "I = import \"inner\";\nexport I") ]
    "{ open (import \"re\"); answer(0) }"

let test_imported_form_calls_unit_macro () =
  macro_in_unit "an imported syntax form calls its unit's macro" 5L
    [ ("forms", "open (import \"std\");\npub macro gen(ts : List(TokenTree)) : List(Decl) { Cons(Syntax.Decl.DeclItems(ts), Nil) };\npub syntax mytype : Decl { mytype $(r : List(TokenTree)) => { gen($r) } }") ]
    "{ M = module { open (import \"forms\"); mytype pub x = 5 }; M.x }"

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
      {|pub rec A = enum { MkA(B), NoA } and B = enum { MkB(A), NoB };
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
    "open (import \"std\");
     pub type A = MkA(B) | NoA and B = MkB(A) | NoB;
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
  Macro_driver.run ~load_syntax:Macro_driver.std_load_syntax (Enforest.parse_module source)

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
      ~elaborate ~eval_and_apply ~syntax_nominals:nominals ~load_syntax:Macro_driver.std_load_syntax source
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
       (exported_kind_with_modules [ ("types_mod", "open (import \"std\");\npub type T = I64") ]
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
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = []; public = false } in
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
  let call = Syntax.MacroCallBinding { f = stx (Syntax.Var (id "gen")); args = []; public = false } in
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

let test_syntax_module_literal_inspector_error () =
  match eval_with_macros "{ macro f(stx) { match (stx) { Syntax.Atom(_) => Syntax.i64(1), _ => Syntax.i64(0) } }; f(g(x)) }" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected atom fallback on non-atom"

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

let test_syntax_expr_nominal_resolvable () =
  let ctx = Elaborate.init_ctx () in
  match Elaborate.resolve_stdlib ctx ["Syntax"; "Expr"] with
  | VNominal { name = "Expr"; num_params = 0; id; captures; _ } ->
      let constructors = Core.nominal_constructors id captures in
      Alcotest.(check int) "one constructor per expression form" 42 (List.length constructors);
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

let test_7g_adt_matching_hygiene_introduced_body () =
  check_i64_macro "7G: rebuilt lambda binds its body through its own parameter" 80L
    "{ macro double(stx) { match (stx) { Syntax.Lam(p, body) => Syntax.RawLam(None, p, quote($body + $body)), _ => Syntax.i64(0) } }; (double(fn(x) { x }))(40) }" ();
  match eval_with_macros
    "{ macro double(stx) { match (stx) { Syntax.Lam(_, body) => quote(fn(x) { $body + $body }), _ => Syntax.i64(0) } }; (double(fn(x) { x }))(40) }"
  with
  | _ -> Alcotest.fail "a string-built binder must not capture the caller's x"
  | exception _ -> ()

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


(* [export M] / [export M.{a, b}]: members join the enclosing module (export-construct). *)
let expect_elab_error label check source =
  match eval_with_macros source with
  | exception Elaborate.ElabError e when check e -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an elaboration error")

let test_export_module_members () =
  check_i64_macro "export M" 3L "{ M = module { pub x = 1; pub y = 2 }; N = module { export M }; N.x + N.y }" ();
  check_i64_macro "export M.{x}" 1L "{ M = module { pub x = 1; pub y = 2 }; N = module { export M.{x} }; N.x }" ();
  expect_elab_error "export M.{x} leaves y out" (fun _ -> true)
    "{ M = module { pub x = 1; pub y = 2 }; N = module { export M.{x} }; N.y }";
  expect_elab_error "export opens nothing locally" (function Elaborate.UnboundVariable _ -> true | _ -> false)
    "{ M = module { pub x = 1 }; N = module { export M; pub z = x }; N.z }"

let test_export_errors () =
  expect_elab_error "export clashes with an own member" (function Elaborate.ExportClash "x" -> true | _ -> false)
    "{ M = module { pub x = 1 }; N = module { pub x = 5; export M }; 0 }";
  expect_elab_error "an own member clashes with an export" (function Elaborate.ExportClash "x" -> true | _ -> false)
    "{ M = module { pub x = 1 }; N = module { export M; pub x = 5 }; 0 }";
  expect_elab_error "two exports clash" (function Elaborate.ExportClash "x" -> true | _ -> false)
    "{ M = module { pub x = 1 }; K = module { pub x = 2 }; N = module { export M; export K }; 0 }";
  expect_elab_error "an unknown exported name" (function Elaborate.ExportUnknownMember "z" -> true | _ -> false)
    "{ M = module { pub x = 1 }; N = module { export M.{z} }; 0 }";
  expect_elab_error "an unnamed impl asks for a name" (function Elaborate.ExportUnnamedImpl "Eq" -> true | _ -> false)
    "{ M = module { pub C = struct { v : I64 }; pub impl Eq(C) = module { fn eq(a, b) { True } } }; N = module { export M }; 0 }"

let test_export_unit_roles () =
  match
    eval_with_imported_macros
      [ ("ops", "pub syntax answer { answer => 42 };\npub x = 1");
        ("reops", "Ops = import \"ops\";\nexport Ops") ]
      "{ R = import \"reops\"; answer + R.x }"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "re-exported role and value" 43L n
  | v -> Alcotest.fail (Printf.sprintf "re-exported role: %s" (Debug.pp_value_short (MetaContext.create ()) v))
  | exception e -> Alcotest.fail (Printf.sprintf "re-exported role: %s" (Printexc.to_string e))

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

let test_decl_hole_kinds_match_parameters () =
  check_i64_macro "a List(Decl) hole takes a group of declarations" 3L
    "{ M = module { syntax both : Decl { both $(ds : List(Decl)) => { $ds } }; both { pub a = 1; pub b = 2 } }; M.a + M.b }" ();
  match eval_with_macros "{ M = module { syntax one : Decl { one $(d : Decl) => { $d } }; one { pub a = 1; pub b = 2 } }; M.a }" with
  | exception Enforest_util.Error msg when String.starts_with ~prefix:"no matching branch" msg -> ()
  | exception e -> Alcotest.fail ("a Decl hole given two declarations: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "a Decl hole must not take two declarations"

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

let expect_expand_error label check source =
  match eval_with_macros source with
  | exception Expand_error.Error { error; _ } when check error -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "%s: %s" label (Printexc.to_string e))
  | _ -> Alcotest.fail (label ^ ": expected an expansion error")

let test_resolved_names_cannot_be_forged () =
  expect_expand_error "a resolved name the macro was not given"
    (function Expand_error.NotSyntax _ -> true | _ -> false)
    "{ x = 5; macro steal(n : Id) { Syntax.RawVar(None, Syntax.Id{name = \"x#0\"; span = None; scope = n.scope}) }; steal(y) }"

let test_token_list_hole () =
  check_i64_macro "the rest of a use and a whole argument, as tokens" 1010L
    "{   M = module {     macro count(ts : List(TokenTree)) : List(Decl) { rec len = fn(l : List(Syntax.TokenTree)) : I64 { match (l) { Nil => 0, Cons(_, t) => 1 + len(t) } }; e = Syntax.i64(len(ts)); quote { pub n = $e } };     syntax tally : Decl { tally $(r : List(TokenTree)) => { count($r) } };     tally A B = X | Y and C = Z   };   N = module {     macro count(ts : List(TokenTree)) : List(Decl) { rec len = fn(l : List(Syntax.TokenTree)) : I64 { match (l) { Nil => 0, Cons(_, t) => 1 + len(t) } }; e = Syntax.i64(len(ts)); quote { pub n = $e } };     count(A B = X | Y and C = Z)   };   M.n * 100 + N.n }" ();
  match eval_with_macros "{ syntax bad : Decl { bad $(r : List(TokenTree)) done => { x = 1 } }; 0 }" with
  | exception Enforest_util.Error _ -> ()
  | _ -> Alcotest.fail "a List(TokenTree) hole before the rule's end was accepted"

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

(* [pub] before a declaration syntax form or a declaration macro call makes every
   declaration it returns public. *)
let pub_forms_unit =
  ("pubforms", "open (import \"std\");
                syntax make : Decl { make $(n : Id) => { $n = 41 } };
                pub make answer;
                macro seven(n : Id) : Decl { Syntax.decl_let(n, Syntax.i64(7), False) };
                pub seven(sev);
                make hidden")

let test_pub_form_uses () =
  check_operator "a pub syntax form's declaration is exported" 42L [ pub_forms_unit ]
    "{ M = import \"pubforms\"; M.answer + 1 }";
  check_operator "a pub macro call's declaration is exported" 7L [ pub_forms_unit ]
    "{ M = import \"pubforms\"; M.sev }";
  match eval_with_imported_macros [ pub_forms_unit ] "{ M = import \"pubforms\"; M.hidden }" with
  | exception _ -> ()
  | _ -> Alcotest.fail "a form used without pub exported its declaration"

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
  let once, ctx = Parse_expand.parse_expr_with_ctx ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source in
  let twice = Expand.expand ctx once in
  Alcotest.(check bool) "expanding expanded syntax renames nothing" true (erase_scopes once = erase_scopes twice)

(* An expression's expansion inside the prelude open, with scopes and source
   positions erased: the two sources differ in length. *)
let expanded_body source =
  let ctx = Elaborate.init_ctx () in
  let elaborate expr = let core, _ = Elaborate.on_expr ctx expr in Elaborate.Ctx.eval ctx core in
  let expr, _ =
    Parse_expand.parse_expr_with_ctx ~elaborate ~eval_and_apply:Nbe.apply_macro
      ~syntax_nominals:(Elaborate.syntax_nominals ctx) ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source
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
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "a module is not a signature" `Quick test_eval_module_is_not_a_signature;
          Alcotest.test_case "imported signature" `Quick test_eval_imported_signature;
          Alcotest.test_case "pi" `Quick test_eval_pi;
          Alcotest.test_case "eq nominal rejected" `Quick test_eval_equality_nominal_rejected;
          Alcotest.test_case "panic message" `Quick (fun () ->
              match eval_source "panic[I64](\"test message\")" with
              | exception Nbe.EvalError "test message" -> ()
              | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
              | _ -> Alcotest.fail "expected panic");
          Alcotest.test_case "unhandled perform" `Quick test_eval_unhandled_perform;
          Alcotest.test_case "unhandled effect at the top is an error" `Quick test_top_unhandled_perform;
          Alcotest.test_case "an escaping closure called at the top is an error" `Quick test_top_escaping_closure;
          Alcotest.test_case "a handled effect may not escape its handler" `Quick test_handled_effect_escape;
          Alcotest.test_case "an imported unit's unhandled effect is an error" `Quick test_top_unhandled_in_imported_unit;
          Alcotest.test_case "a method is pure unless it declares a row" `Quick test_method_rows;
          Alcotest.test_case "a trait method signature carries a row" `Quick test_trait_method_rows;
          Alcotest.test_case "type-case default String panics" `Quick test_eval_type_case_default_string_panics;
          Alcotest.test_case "continuation reuse error" `Quick test_eval_continuation_reuse_error;
           Alcotest.test_case "addition overflow is a language error" `Quick
             (check_overflow "add" "+" "{ 9223372036854775807 + 1 }");
           Alcotest.test_case "subtraction overflow is a language error" `Quick
             (check_overflow "sub" "-" "{ (0 - 9223372036854775807 - 1) - 1 }");
           Alcotest.test_case "multiplication overflow is a language error" `Quick
             (check_overflow "mul" "*" "{ 4611686018427387904 * 2 }");
           Alcotest.test_case "min_int / -1 overflows" `Quick
             (check_overflow "div" "/" "{ (0 - 9223372036854775807 - 1) / (0 - 1) }");
           Alcotest.test_case "division by zero is a language error" `Quick
             (check_div_by_zero "division by zero" "{ 1 / 0 }");
           Alcotest.test_case "remainder by zero is a language error" `Quick
             (check_div_by_zero "remainder by zero" "{ 1 % 0 }");
           Alcotest.test_case "division by zero through a call" `Quick
             (check_div_by_zero "division by zero through a call"
                "{ f = fn(x: I64) { 1 / x }; f(0) }");
        ] );
      ( "module-level open",
        [
            (* The open extends the runtime scope of the bindings that follow
               it, so every de Bruijn index in the module — for names bound
               before the open as well as after it — must still line up. *)
        ] );
      ( "imports",
        [
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
          Alcotest.test_case "export a unit macros" `Quick test_export_unit_macros;
          Alcotest.test_case "imported form calls unit macro" `Quick test_imported_form_calls_unit_macro;
          Alcotest.test_case "bare import does not inject macros" `Quick test_bare_import_does_not_inject_macros;
          Alcotest.test_case "open delivers macros bare" `Quick test_open_delivers_macros_bare;
          Alcotest.test_case "open bound import delivers macros bare" `Quick test_open_bound_import_delivers_macros_bare;
          Alcotest.test_case "same macro name in two units" `Quick test_same_macro_name_in_two_units;
          Alcotest.test_case "imported macro not runtime field" `Quick test_imported_macro_not_runtime_field;
          Alcotest.test_case "imported macro circular visit" `Quick test_imported_macro_circular_visit;
          Alcotest.test_case "macro-generated import loads macros" `Quick test_macro_generated_import_loads_macros;
          Alcotest.test_case "macro-generated import checks missing" `Quick test_macro_generated_import_checks_missing;
          Alcotest.test_case "imported macro calls regular function" `Quick test_imported_macro_calls_regular_function;
          Alcotest.test_case "macro Decl in Expr context rejected" `Quick test_macro_decl_in_expr_context;
          Alcotest.test_case "Decl kind survives elaboration" `Quick test_decl_kind_registered_persists;
          Alcotest.test_case "Decl macro generates binding in module" `Quick test_decl_macro_generates_binding;
          Alcotest.test_case "imported Decl macro generates binding" `Quick test_imported_decl_macro;
          Alcotest.test_case "Decl macro two calls" `Quick test_decl_macro_two_calls;
          Alcotest.test_case "type-aware checking mode" `Quick test_type_aware_checking;
          Alcotest.test_case "macro does not capture its argument" `Quick test_macro_does_not_capture_argument;
          Alcotest.test_case "macro body sees nothing ambient" `Quick test_macro_body_sees_nothing_ambient;
          Alcotest.test_case "quote splices holes" `Quick test_quote_splices_holes;
          Alcotest.test_case "template literals resolve at definition" `Quick test_template_literals_resolve_at_definition;
          Alcotest.test_case "block-local macros do not leak" `Quick test_block_local_macros_do_not_leak;
          Alcotest.test_case "reflection round trip is the identity" `Quick test_round_trip_is_identity;
          Alcotest.test_case "unit-level type chain" `Quick test_unit_level_type_chain;
          Alcotest.test_case "type-directed default Bool" `Quick test_type_default_bool;
          Alcotest.test_case "R-type match non-exhaustive missing ctors" `Quick test_rtype_match_non_exhaustive_missing_ctors;
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
          Alcotest.test_case "operator macro error reports spans" `Quick test_operator_macro_error_reports_spans;
          Alcotest.test_case "macro body budget overrun names the macro" `Quick test_macro_body_budget_overrun_names_the_macro;
          Alcotest.test_case "operator body error reports use span" `Quick test_operator_body_error_reports_use_span;
          Alcotest.test_case "operator body panic reports use span" `Quick
            (operator_body_failure_reports_use_span "panic[Syntax.Expr](\"boom\")" "boom");
          Alcotest.test_case "operator body division by zero reports use span" `Quick
            (* The divisor waits on [stx], so the checker cannot evaluate it at
               the definition: the division happens in the application. *)
            (operator_body_failure_reports_use_span "{ _ = 1 / (match (stx) { _ => 0 }); stx }" "division by zero");
          Alcotest.test_case "Syntax module: literal inspector error" `Quick test_syntax_module_literal_inspector_error;
          Alcotest.test_case "Syntax module: ap deconstructor error" `Quick test_syntax_module_ap_deconstructor_error;
          Alcotest.test_case "Syntax module: lam deconstructor error" `Quick test_syntax_module_lam_deconstructor_error;
          Alcotest.test_case "Syntax module: let deconstructor error" `Quick test_syntax_module_let_deconstructor_error;
          Alcotest.test_case "Syntax module: primitive names hidden" `Quick test_syntax_primitive_names_hidden;
          Alcotest.test_case "Syntax module: Expr nominal resolvable" `Quick test_syntax_expr_nominal_resolvable;
          Alcotest.test_case "pattern synonym substitution" `Quick test_pattern_syn_subst;
          Alcotest.test_case "pattern synonym in prelude" `Quick test_pattern_syn_in_prelude;
          Alcotest.test_case "7G: ADT destructured body not captured by outer scope" `Quick test_7g_adt_matching_hygiene_introduced_body;
          Alcotest.test_case "7I: generated syntax obeys later-wins shadowing" `Quick test_7i_generated_syntax_later_wins_shadow;
          Alcotest.test_case "imported operator prefix expands" `Quick test_imported_operator_prefix_expands;
          Alcotest.test_case "imported syntax not runtime field" `Quick test_imported_syntax_not_runtime_field;
          Alcotest.test_case "syntax template: capture extent" `Quick test_syntax_template_capture_extent;
          Alcotest.test_case "order group errors" `Quick test_order_group_errors;
          Alcotest.test_case "order group imported" `Quick test_order_group_imported;
          Alcotest.test_case "syntax template: imported pub syntax" `Quick test_syntax_template_imported_pub_syntax;
          Alcotest.test_case "syntax template: imported intro binding hygiene" `Quick test_syntax_template_imported_intro_binding_hygiene;
          Alcotest.test_case "decl hole kinds match parameter kinds" `Quick test_decl_hole_kinds_match_parameters;
          Alcotest.test_case "decl template: multi generates siblings" `Quick test_decl_template_multi_generates_siblings;
          Alcotest.test_case "decl template: multi rejected in expr" `Quick test_decl_template_multi_rejected_in_expr;
          Alcotest.test_case "decl template: struct field deferred" `Quick test_decl_template_struct_field_deferred;
          Alcotest.test_case "7I: generated pub syntax across imports" `Quick test_7i_generated_pub_syntax_across_imports;
          Alcotest.test_case "7I: generated pub operator across imports" `Quick test_7i_generated_pub_operator_across_imports;
          Alcotest.test_case "7I: generated pub macro across imports" `Quick test_7i_generated_pub_macro_across_imports;
          Alcotest.test_case "7I: generated pub operator rejected in struct" `Quick test_7i_generated_pub_operator_rejected_in_struct;
          Alcotest.test_case "7I: generated pub macro rejected in struct" `Quick test_7i_generated_pub_macro_rejected_in_struct;
          Alcotest.test_case "7I: generated syntax cycle" `Quick test_7i_generated_syntax_cycle;
          Alcotest.test_case "7I: generated macro cycle" `Quick test_7i_generated_macro_cycle;
        ] );
      ( "m7 roles",
        [
          Alcotest.test_case "template-written syntax invisible to user" `Quick test_m7_template_written_syntax_invisible;
          Alcotest.test_case "value binder under syntax" `Quick test_m7_value_binder_under_syntax;
          Alcotest.test_case "syntax after value binder" `Quick test_m7_syntax_after_value_binder;
          Alcotest.test_case "fn param under syntax" `Quick test_m7_fn_param_under_syntax;
          Alcotest.test_case "match binder under syntax" `Quick test_m7_match_binder_under_syntax;
          Alcotest.test_case "syntax inside param region" `Quick test_m7_syntax_inside_param_region;
          Alcotest.test_case "value under macro" `Quick test_m7_value_under_macro;
          Alcotest.test_case "value under imported operator" `Quick test_m7_value_under_imported_operator;
          Alcotest.test_case "new binder under attached fixity" `Quick test_m7_new_binder_under_attached_fixity;
          Alcotest.test_case "open under syntax" `Quick test_m7_open_under_syntax;
          Alcotest.test_case "syntax inside open region" `Quick test_m7_syntax_inside_open_region;
          Alcotest.test_case "imported role in its open's region" `Quick test_m7_import_open_role_in_region;
          Alcotest.test_case "block open (import) role not after block" `Quick test_m7_import_open_role_not_after_block;
          Alcotest.test_case "block import binder role not after block" `Quick test_m7_import_binder_role_not_after_block;
          Alcotest.test_case "import open under unit syntax" `Quick test_m7_import_open_under_syntax;
          Alcotest.test_case "driver-run open under syntax" `Quick test_m7_driver_open_under_syntax;
        ] );
      ( "m9 forms",
        [
          Alcotest.test_case "expansion is idempotent" `Quick test_m9_expansion_idempotent;
          Alcotest.test_case "filling equals the quote" `Quick test_m9_filling_equals_quote;
          Alcotest.test_case "pub form uses export" `Quick test_pub_form_uses;
          Alcotest.test_case "List(TokenTree) holes" `Quick test_token_list_hole;
          Alcotest.test_case "export a module's members" `Quick test_export_module_members;
          Alcotest.test_case "export errors" `Quick test_export_errors;
          Alcotest.test_case "export a unit's roles" `Quick test_export_unit_roles;
          Alcotest.test_case "an argument of the wrong kind" `Quick test_m9_param_kind_mismatch;
          Alcotest.test_case "resolved names cannot be forged" `Quick test_resolved_names_cannot_be_forged;
          Alcotest.test_case "a Decl argument of the wrong kind" `Quick test_m9_param_decl_kind_mismatch;
          Alcotest.test_case "a Decl parameter is one declaration" `Quick test_m9_param_one_decl;
          Alcotest.test_case "expand_decls budget" `Quick test_expand_decls_budget;
          Alcotest.test_case "expand_decls imported" `Quick test_expand_decls_imported;
          Alcotest.test_case "an imported macro's parameter kinds" `Quick test_m9_param_imported;
          Alcotest.test_case "a macro is given exactly its arguments" `Quick test_m8_argument_count;
          Alcotest.test_case "an imported macro's argument count" `Quick test_m8_imported_argument_count;
          Alcotest.test_case "a Decl macro's output type" `Quick test_decl_macro_output_type;
        ] );
    ]
