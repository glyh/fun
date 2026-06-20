open Core
open Atom
open Nbe
open Unify

let mc () = MetaContext.create ()
let pure_effects = effect_row_closure [] empty_effect_row

let parse_expr source =
  Parse_expand.parse_expr source

let fail_with_source label source message =
  Alcotest.fail (Printf.sprintf "%s: %s\nsource:\n%s" label message source)

let eval_source source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  let core, _ty = Elaborate.on_expr ctx expr in
  Elaborate.Ctx.eval ctx core

let eval_source_with_loader loader source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  let core, _ty = Elaborate.on_expr ~loader ctx expr in
  Elaborate.Ctx.eval ctx core

let with_modules modules f =
  let dir = Filename.temp_dir "fun_core_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = Core_loader.create ~base_dir:dir in
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
  | VAtom (Bool b) -> Alcotest.(check bool) label expected b
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
  match eval_source "(1, true)" with
  | VProd [ VAtom (I64 1L); VAtom (Bool true) ] -> ()
  | _ -> Alcotest.fail "expected VProd"

let test_eval_pi () =
  match eval_source "I64 -> Bool" with
  | VPi { domain = VAtomTy Atom_ty.TI64; _ } -> ()
  | _ -> Alcotest.fail "expected VPi"

let test_eval_dot () =
  match eval_source "do M = module pub x = 99 end; M.x end" with
  | VAtom (I64 n) -> Alcotest.(check int64) "dot" 99L n
  | _ -> Alcotest.fail "expected VAtom"

let test_eval_module_signature_argument () =
  check_i64 "module signature argument" 42L
    "(fn(m : module pub x = I64 end) -> m.x)(module pub x = 42 end)"
    ()

let test_eval_module_signature_extra_field () =
  check_i64 "module signature extra field" 42L
    "(fn(m : module pub x = I64 end) -> m.x)(module pub x = 42; pub y = true end)"
    ()

let test_eval_signature_sugar_argument () =
  check_i64 "signature sugar argument" 42L
    "(fn(m : sig x : I64 end) -> m.x)(module pub x = 42 end)"
    ()

let test_eval_module_signature_functor () =
  check_i64 "module signature functor" 42L
    "do F = fn(M : module pub x = I64 end) -> module pub doubled = M.x + M.x end; F(module pub x = 21 end).doubled end"
    ()

let test_ref_read_initial () =
  check_i64 "ref read initial" 1L "do r = ref(1); deref(r) end" ()

let test_ref_write_read () =
  check_i64 "ref write read" 2L "do r = ref(1); _ = r <- 2; deref(r) end" ()

let test_ref_aliases_share_cell () =
  check_i64 "ref aliases share cell" 3L "do r = ref(1); alias = r; _ = alias <- 3; deref(r) end" ()

let test_ref_closure_observes_later_write () =
  check_i64 "ref closure observes later write" 4L "do r = ref(0); f = fn(_) -> deref(r); _ = r <- 4; f() end" ()

let test_ref_repeated_closure_increments () =
  check_i64 "ref repeated closure increments" 2L
    "do r = ref(0); inc = fn(_) -> do n = deref(r); _ = r <- n + 1; deref(r) end; _ = inc(); inc() end"
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
  let v1 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TBool } } in
  let v2 = VPi { explicitness = Explicit; domain = VAtomTy Atom_ty.TI64; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TBool } } in
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
      let result = closure_apply mc clo (VAtomTy Atom_ty.TBool) in
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
          let result = closure_apply mc inner_clo (VAtomTy Atom_ty.TBool) in
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
          let result = closure_apply mc inner_clo (VAtomTy Atom_ty.TBool) in
          Alcotest.(check bool) "rename snd" true
            (match result with VAtomTy Atom_ty.TBool -> true | _ -> false)
      | _ -> Alcotest.fail "expected inner VLam")
  | _ -> Alcotest.fail "expected VLam"

let test_unify_occurs_check () =
  let mc = mc () in
  let id = MetaContext.fresh mc in
  let v1 = VFlex { id; spine = [] } in
  let v2 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy Atom_ty.TBool } } in
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
  let v2 = VAtomTy Atom_ty.TBool in
  match unify mc [] 0 v1 v2 with
  | exception UnifyError _ -> ()
  | _ -> Alcotest.fail "expected unify error"

let test_unify_nominal_params () =
  let mc = mc () in
  let nom1 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy Atom_ty.TI64 ]; constructors = [] } in
  let nom2 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy Atom_ty.TBool ]; constructors = [] } in
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
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy Atom_ty.TBool ]; operations = [] } in
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
  | exception Nbe.EvalError msg ->
      if not (String.contains msg 'S') then Alcotest.fail ("unexpected perform error: " ^ msg)
  | _ -> Alcotest.fail "expected unhandled perform error"

let test_debug_perform () =
  let text = Debug.pp_term (Perform { eff = EffectRef ("State", [ AtomTy Atom_ty.TI64 ]); op = "get"; arg = Atom Unit }) in
  if not (String.contains text 'g') then Alcotest.fail ("expected perform debug output, got " ^ text)

let test_eval_handler_ignores_continuation () =
  check_i64 "handler ignores continuation" 2L
    "do effect Exc = sig raise : I64 -> I64 end; match perform Exc.raise(1) do x -> x | effect Exc.raise n -> n + 1 end end"
    ()

let test_eval_handler_resumes_once () =
  check_i64 "handler resumes once" 2L
    "do effect Exc = sig raise : I64 -> I64 end; match perform Exc.raise(1) do x -> x | effect Exc.raise n -> resume(n + 1) end end"
    ()

let test_eval_handler_value_branch () =
  check_i64 "handler value branch" 42L
    "do effect Exc = sig raise : I64 -> I64 end; match 41 do x -> x + 1 | effect Exc.raise n -> 0 end end"
    ()

let test_eval_handler_outer_bubble () =
  check_i64 "handler outer bubble" 2L
    "do effect Exc = sig raise : I64 -> I64 end; match (match perform Exc.raise(1) do x -> x end) do x -> x | effect Exc.raise n -> n + 1 end end"
    ()

let test_eval_handler_escape_skips_continuation () =
  check_i64 "handler escape skips continuation" 99L
    "do
       effect Exit = sig now : I64 -> I64 end
       program : Unit -> I64 can Exit = fn(_) -> do
         _ = perform Exit.now(99)
         0
       end
       match program() do x -> x
       | effect Exit.now value -> value
       end
     end"
    ()

let test_eval_handler_ping_pong_effects () =
  check_i64 "handler ping pong effects" 212L
    "do
       effect Ping = sig hit : I64 -> I64 end
       effect Pong = sig hit : I64 -> I64 end
       program : Unit -> I64 can {Ping, Pong} = fn(_) -> do
         x = perform Ping.hit(1)
         perform Pong.hit(x + 10)
       end
       match program() do x -> x
       | effect Ping.hit n -> do y = perform Pong.hit(n + 1); resume(y) end
       | effect Pong.hit n -> resume(n + 100)
       end
     end"
    ()

let test_eval_recursive_handler_ping_pong_effects () =
  check_i64 "recursive handler ping pong effects" 10L
    "do
       effect Ping = sig hit : I64 -> I64 end
       effect Pong = sig hit : I64 -> I64 end
       rec loop : I64 -> I64 can {Ping, Pong} = fn(n) do
          if n == 0 do
            0
          else
            do x = perform Ping.hit(n); y = perform Pong.hit(n - 1); x + y + loop(n - 2) end
          end
        end
        match loop(4) do x -> x
       | effect Ping.hit n -> resume(n)
       | effect Pong.hit n -> resume(n)
       end
     end"
    ()

let test_eval_state_handler_sequences_operations () =
  check_i64 "state handler sequences operations" 2L
    "do
       effect State(S) = sig get : Unit -> S; put : S -> Unit end
       program : Unit -> I64 can State(I64) = fn(_) -> do
         x = perform State.get()
         _ = perform State.put(x + 1)
         perform State.get()
       end
        rec run : I64 -> (Unit -> I64 can State(I64)) -> I64 = fn(state, thunk) do
          match thunk() do x -> x
          | effect State.get () -> run(state, fn(_) -> resume(state))
          | effect State.put next -> run(next, fn(_) -> resume())
          end
        end
        run(1, program)
     end"
    ()

let test_eval_handler_tuple_payload_pattern () =
  check_i64 "handler tuple payload pattern" 42L
    "do effect Console = sig log : I64 * I64 -> I64 end; match perform Console.log((40, 2)) do x -> x | effect Console.log (level, message) -> level + message end end"
    ()

let test_eval_handler_tuple_payload_binding_order () =
  check_i64 "handler tuple payload binding order" 38L
    "do effect Console = sig log : I64 * I64 -> I64 end; match perform Console.log((40, 2)) do x -> x | effect Console.log (level, message) -> level - message end end"
    ()

let test_eval_handler_record_payload_pattern () =
  check_i64 "handler record payload pattern" 42L
    "do
       Request = struct value: I64; extra: I64; end
       effect Ask = sig prompt : Request -> I64 end
       match perform Ask.prompt(Request{value = 40; extra = 2}) do x -> x
       | effect Ask.prompt Request{value; extra} -> value + extra
       end
     end"
    ()

let test_eval_handler_record_payload_binding_order () =
  check_i64 "handler record payload binding order" 38L
    "do
       Request = struct value: I64; extra: I64; end
       effect Ask = sig prompt : Request -> I64 end
       match perform Ask.prompt(Request{value = 40; extra = 2}) do x -> x
       | effect Ask.prompt Request{value; extra} -> value - extra
       end
     end"
    ()

let is_zeroish_source call =
  "do
     is_zeroish : [T : Type] -> T -> Bool = fn[T : Type](x) do
       match T do I64 -> x == 0
       | Bool -> x == false
       | Unit -> true
       | Char -> x == 'a'
       | _ -> false
       end
     end
     " ^ call ^ "
   end"

let test_eval_type_case_i64_zero () =
  check_bool "type-case I64 zero" true (is_zeroish_source "is_zeroish(0)") ()

let test_eval_type_case_i64_nonzero () =
  check_bool "type-case I64 nonzero" false (is_zeroish_source "is_zeroish(1)") ()

let test_eval_type_case_bool_false () =
  check_bool "type-case Bool false" true (is_zeroish_source "is_zeroish(false)") ()

let test_eval_type_case_bool_true () =
  check_bool "type-case Bool true" false (is_zeroish_source "is_zeroish(true)") ()

let test_eval_type_case_unit () =
  check_bool "type-case Unit" true (is_zeroish_source "is_zeroish(())") ()

let test_eval_type_case_char_a () =
  check_bool "type-case Char a" true (is_zeroish_source "is_zeroish('a')") ()

let default_source call =
  "do
     default : [T : Type] -> T = fn[T : Type] do
       match T do I64 -> 0
       | Bool -> false
       | Unit -> ()
       | Char -> 'a'
       | _ -> panic(\"no default\")
       end
     end
     " ^ call ^ "
   end"

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
  "do
     default_or : [T : Type] -> T -> T = fn[T : Type](fallback) do
       match T do I64 -> 0
       | Bool -> false
       | Unit -> ()
       | Char -> 'a'
       | String -> \"\"
       | _ -> fallback
       end
     end
     " ^ call ^ "
   end"

let test_eval_type_case_default_or_i64 () =
  check_i64 "type-case default_or I64" 0L (default_or_source "default_or[I64](99)") ()

let test_eval_type_case_default_or_bool () =
  check_bool "type-case default_or Bool" false (default_or_source "default_or[Bool](true)") ()

let test_eval_type_case_default_or_string () =
  check_bool "type-case default_or String" true (default_or_source "default_or[String](\"fallback\") == \"\"") ()

let test_eval_type_case_default_or_nominal_fallback () =
  check_i64 "type-case default_or nominal fallback" 2L
    (default_or_source "type Color = Red | Blue; match default_or[Color](Blue) do Red -> 1 | Blue -> 2 end")
    ()

let type_name_source call =
  "do
     type_name : Type -> String = fn(T) do
       match T do I64 -> \"i64\"
       | Bool -> \"bool\"
       | Char -> \"char\"
       | Unit -> \"unit\"
       | String -> \"string\"
       | _ -> \"other\"
       end
     end
     " ^ call ^ "
   end"

let test_eval_type_case_type_name_i64 () =
  check_bool "type-case type_name I64" true (type_name_source "type_name(I64) == \"i64\"") ()

let test_eval_type_case_type_name_string () =
  check_bool "type-case type_name String" true (type_name_source "type_name(String) == \"string\"") ()

let test_eval_equality_nominal_rejected () =
  match eval_source "do type Color = Red; Red == Red end" with
  | exception Elaborate.ElabError (UnknownTrait "Eq") -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected missing Eq impl"

let test_eval_type_case_nominal_full_application () =
  check_i64 "type-case nominal full application" 1L
    "do type Option a = Some a | None
     match Option(I64) do Option(I64) -> 1
     | Option(Bool) -> 2
     | _ -> 3
     end
     end"
    ()

let test_eval_type_case_nominal_param_bind () =
  check_i64 "type-case nominal param bind" 1L
    "do type Option a = Some a | None
     match Option(I64) do Option x -> match x do I64 -> 1 | _ -> 2 end
     | _ -> 3
     end
     end"
    ()

let test_eval_type_case_nominal_complex_param_pattern () =
  check_i64 "type-case nominal complex param pattern" 1L
    "do type Option a = Some a | None
     match Option(Option(I64)) do Option(Option(I64) | I64) -> 1
     | Option _ -> 2
     | _ -> 3
     end
     end"
    ()

let nominal_classify_source call =
  "do
     type Option a = Some a | None
     classify : Type -> I64 = fn(T) do
       match T do Option(I64) -> 1
       | Option _ -> 2
       | _ -> 0
       end
     end
     " ^ call ^ "
   end"

let test_eval_type_case_nominal_classifier_i64 () =
  check_i64 "type-case nominal classifier I64" 1L (nominal_classify_source "classify(Option(I64))") ()

let test_eval_type_case_nominal_classifier_bool () =
  check_i64 "type-case nominal classifier Bool" 2L (nominal_classify_source "classify(Option(Bool))") ()

let test_eval_type_case_nominal_classifier_fallback () =
  check_i64 "type-case nominal classifier fallback" 0L (nominal_classify_source "classify(I64)") ()

let struct_type_classify_source call =
  "do
     classify : Type -> I64 = fn(T) do
       match T do struct x: I64; _ end -> 1
       | struct x: Bool; _ end -> 2
       | struct y: p; _ end -> match p do String -> 3 | _ -> 4 end
       | _ -> 0
       end
     end
     " ^ call ^ "
   end"

let test_eval_type_case_struct_field_i64 () =
  check_i64 "type-case struct field I64" 1L
    (struct_type_classify_source "type Point = {x: I64; y: Bool}; classify(Point)")
    ()

let test_eval_type_case_struct_field_bool () =
  check_i64 "type-case struct field Bool" 2L
    (struct_type_classify_source "type Point = {x: Bool}; classify(Point)")
    ()

let test_eval_type_case_struct_field_binder () =
  check_i64 "type-case struct field binder" 3L
    (struct_type_classify_source "type Point = {y: String; z: I64}; classify(Point)")
    ()

let test_eval_type_case_struct_field_fallback () =
  check_i64 "type-case struct field fallback" 0L (struct_type_classify_source "classify(I64)") ()

let test_eval_type_case_struct_closed_rejects_extra () =
  check_i64 "type-case struct closed rejects extra" 2L
    "do type Point = {x: I64; y: Bool}
     match Point do struct x: I64 end -> 1
     | struct x: I64; _ end -> 2
     | _ -> 3
     end
     end"
    ()

let test_eval_handler_same_match_branch_effect () =
  check_i64 "handler same match branch effect" 43L
    "do effect Ping = sig hit : I64 -> I64 end
     match perform Ping.hit(1) do x -> x
     | effect Ping.hit n ->
         if n == 1 do
           do y = perform Ping.hit(42); y + 1 end
          else
            resume(n)
          end
     end
     end"
    ()

let test_eval_handler_parameterized_dispatch () =
  check_i64 "handler parameterized dispatch" 11L
    "do
      effect State(S) = sig get : Unit -> S end
     StateI64 = State(I64)
     StateBool = State(Bool)
     match (if perform StateBool.get(()) do perform StateI64.get(()) + 1 else 0 end) do x -> x
     | effect StateI64.get () -> resume(10)
     | effect StateBool.get () -> resume(true)
     end
     end"
    ()

let test_eval_handler_value_branch_handles_same_effect () =
  check_i64 "handler value branch handles same effect" 42L
    "do effect Ask = sig value : Unit -> I64 end
     match 0 do x -> perform Ask.value(())
     | effect Ask.value () -> 42
     end
     end"
    ()

let test_eval_handler_value_branch_bubbles_outer_effect () =
  check_i64 "handler value branch bubbles outer effect" 42L
    "do
      effect Inner = sig value : Unit -> I64 end
      effect Outer = sig value : Unit -> I64 end
     match (match 0 do x -> perform Outer.value(())
       | effect Inner.value () -> 0
       end) do x -> x
     | effect Outer.value () -> 42
     end
     end"
    ()

let test_eval_handler_resumed_continuation_is_deep () =
  check_i64 "handler resumed continuation is deep" 42L
    "do effect Ping = sig hit : I64 -> I64 end
     match (if perform Ping.hit(1) == 41 do perform Ping.hit(2) else 0 end) do x -> x
     | effect Ping.hit n -> resume(n + 40)
     end
     end"
    ()

let test_eval_handler_outer_handles_residual_effect () =
  check_i64 "handler outer handles residual effect" 42L
    "do
      effect Inner = sig hit : I64 -> I64 end
      effect Outer = sig hit : I64 -> I64 end
     match (match perform Outer.hit(1) do x -> x
       | effect Inner.hit n -> resume(n)
       end) do x -> x
     | effect Outer.hit n -> n + 41
     end
     end"
    ()

let test_eval_handler_lexical_resume_nested_lambda () =
  check_i64 "handler lexical resume nested lambda" 41L
    "do effect Exc = sig raise : I64 -> I64 end
     match perform Exc.raise(1) do x -> x
     | effect Exc.raise n -> (fn(x) -> resume(x + 40))(n)
     end
     end"
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

let eval_with_macros source =
  let ctx = Elaborate.init_ctx () in
  let nominals =
    { Macro_eval.expr = Elaborate.resolve_stdlib ctx ["Syntax"; "Expr"];
      explicitness = Elaborate.resolve_stdlib ctx ["Syntax"; "Explicitness"];
      atom_val = Elaborate.resolve_stdlib ctx ["Syntax"; "AtomVal"];
      option_ = Elaborate.resolve_stdlib ctx ["Option"] }
  in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = MetaContext.create () in
    Nbe.apply mc fn arg
  in
  let expr = Parse_expand.parse_expr ~elaborate ~eval_and_apply ~syntax_nominals:nominals source in
  let core, _ty = Elaborate.on_expr ctx expr in
  Elaborate.Ctx.eval ctx core

let eval_with_imported_macros modules source =
  with_modules modules (fun loader ->
      let elaborate expr =
        let ctx = Elaborate.init_ctx () in
        let core, _ty = Elaborate.on_expr ~loader ctx expr in
        Elaborate.Ctx.eval ctx core
      in
      let eval_and_apply fn arg =
        let mc = MetaContext.create () in
        Nbe.apply mc fn arg
      in
      let expr =
        Parse_expand.parse_expr
          ~elaborate
          ~eval_and_apply
          ~load_macros:(Core_loader.visit_macros loader)
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          source
      in
      let ctx = Elaborate.init_ctx () in
      let core, _ty = Elaborate.on_expr ~loader ctx expr in
      Elaborate.Ctx.eval ctx core)

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
    "do x = 1; macro m(_) -> Syntax.lam(\"x\", Syntax.var(\"x\")); (m @ (0))(x) end" ()

let test_macro_hygiene_user_no_capture_macro () =
  check_i64_macro "no capture" 1L
    "do macro m(_) -> Syntax.lam(\"x\", Syntax.var(\"x\")); x = 1; (m @ (0))(x) end" ()

let test_macro_panic_has_message () =
  match eval_with_macros "do macro bad(_) -> panic[I64](\"boom\"); bad @ (0) end" with
  | exception EvalError msg ->
      Alcotest.(check bool) "panic message contains 'boom'" true (String.contains msg 'b')
  | _ -> Alcotest.fail "expected panic"

let test_imported_macro_expands () =
  match
    eval_with_imported_macros
      [ ("macros", "pub macro answer(_) -> Syntax.i64(42)") ]
      "do M = import \"macros\"; answer @ (0) end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported macro" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "imported macro: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "imported macro: %s" (Printexc.to_string e))

let test_imported_macro_not_runtime_field () =
  match
    eval_with_imported_macros
      [ ("macros", "pub macro answer(_) -> Syntax.i64(42)") ]
      "do M = import \"macros\"; M.answer end"
  with
  | exception Elaborate.ElabError _ -> ()
  | exception Nbe.EvalError _ -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected imported macro to be compile-time only"

let test_imported_macro_circular_visit () =
  match
    eval_with_imported_macros
      [ ("a", "pub macro ma(_) -> do B = import \"b\"; Syntax.i64(1) end");
        ("b", "pub macro mb(_) -> do A = import \"a\"; Syntax.i64(2) end") ]
      "do A = import \"a\"; ma @ (0) end"
  with
  | exception Core_loader.CircularMacroVisit "a" -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected circular macro visit"

let test_macro_generated_import_loads_macros () =
  match
    eval_with_imported_macros
      [ ("loader", "pub macro through(stx) -> stx");
        ("target", "pub macro answer(_) -> Syntax.i64(42)") ]
      "do L = import \"loader\"; T = through @ (import \"target\"); answer @ (0) end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "macro-generated import" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "macro-generated import: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "macro-generated import: %s" (Printexc.to_string e))

let test_macro_generated_import_checks_missing () =
  match
    eval_with_imported_macros
      [ ("loader", "pub macro through(stx) -> stx") ]
      "do L = import \"loader\"; through @ (import \"missing\") end"
  with
  | exception Core_loader.ImportNotFound "missing" -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected missing macro-generated import"

let test_imported_macro_calls_regular_function () =
  match
    eval_with_imported_macros
      [ ("helper", "pub make_answer = fn(stx) -> Syntax.i64(42)");
        ("macros", "pub macro answer(stx) -> do H = import \"helper\"; H.make_answer(stx) end") ]
      "do M = import \"macros\"; answer @ (0) end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "macro calls function" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "macro calls function: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "macro calls function: %s" (Printexc.to_string e))

let test_operator_prefix_macro_expands () =
  check_i64_macro "operator prefix" 42L
    "do
       syntax answer do | answer -> 42 end
       answer
     end" ()

let test_operator_infix_macro_expands () =
  check_i64_macro "infix macro expands" 9L
    "do
       infix (~) 15 Left (stx) -> Syntax.i64(9)
       1 ~ 2
       end" ()

let test_operator_uses_operands () =
  check_i64_macro "operator uses operands (Left assoc)" 2L
    "do
       infix (>>>) 15 Left ($lhs, $rhs) -> $lhs - $rhs
       10 >>> 5 >>> 3
     end" ()

let test_operator_rhs_can_use_earlier_macro () =
  check_i64_macro "operator RHS macro path" 5L
    "do
       macro answer_body(_) -> Syntax.i64(5)
       syntax answer do | answer -> answer_body @ (0) end
       answer
      end" ()

let test_operator_prefix_receives_structured_input () =
  check_i64_macro "syntax template reuses hole" 4L
    "do
       syntax twice do | twice $x -> $x + $x end
       twice 2
      end" ()

let test_operator_macro_error_reports_spans () =
  match
    eval_with_macros
       "do
          infix (~) 15 Left (stx) -> 1
          1 ~ 2
        end"
  with
  | exception Failure msg ->
      Alcotest.(check bool) "mentions operator" true (string_contains msg "syntax operator");
      Alcotest.(check bool) "mentions use span" true (string_contains msg "used at");
      Alcotest.(check bool) "mentions declaration span" true (string_contains msg "declared at")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected syntax operator macro expansion failure"

let test_syntax_module_expression_kind () =
  check_i64_macro "Syntax.kind expression object" 1L
    "do macro answer(stx) -> match stx do | Syntax.Var(_) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; x = 10; answer @ (x) end" ()

let test_syntax_module_literal_inspectors () =
  check_i64_macro "Syntax literal inspectors" 42L
    "do macro answer(_) -> Syntax.i64(42); answer @ () end" ()

let test_syntax_module_literal_inspector_error () =
  match eval_with_macros "do macro f(stx) -> match stx do Syntax.Atom(_) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; f @ (Syntax.var(\"x\")) end" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected atom fallback on non-atom"

let test_syntax_module_ap_deconstructors () =
  check_i64_macro "Syntax ap deconstructors" 1L
    "do macro f(stx) -> match stx do | Syntax.Ap(f, a) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; f @ (add(1, 2)) end" ()

let test_syntax_module_ap_deconstructor_error () =
  match eval_with_macros "do macro f(stx) -> match stx do Syntax.Ap(_, _) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; f @ (1) end" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected ap fallback on non-ap"

let test_syntax_module_lam_deconstructor_error () =
  match eval_with_macros "do macro f(stx) -> match stx do Syntax.Lam(_, _) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; f @ (Syntax.i64(0)) end" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected lam fallback on non-lam"

let test_syntax_module_let_deconstructor_error () =
  match eval_with_macros "do macro f(stx) -> match stx do Syntax.Let(_, _, _) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; f @ (Syntax.i64(0)) end" with
  | VAtom (I64 0L) -> ()
  | _ -> Alcotest.fail "expected let fallback on non-let"

let test_syntax_module_identifier_inspection () =
  check_i64_macro "Syntax identifier inspection" 1L
    "do macro inspect(stx) -> match stx do Syntax.Var(_) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; target = 10; inspect @ (target) end" ()


let test_syntax_module_i64_builder () =
  check_i64_macro "Syntax.i64 builder" 42L
    "do
       macro answer(_) -> Syntax.i64(42)
       answer @ (0)
     end" ()

let test_syntax_class_types_accessible () =
  check_i64_macro "Syntax class types accessible" 42L
    "do
       x = Syntax.i64(42)
       42
     end" ()

let test_syntax_expr_nominal_resolvable () =
  let ctx = Elaborate.init_ctx () in
  match Elaborate.resolve_stdlib ctx ["Syntax"; "Expr"] with
  | VNominal { name = "Expr"; num_params = 0; constructors; _ } ->
      Alcotest.(check int) "5 constructors" 5 (List.length constructors);
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
  match eval_with_macros "do macro answer(_) -> stx_make_i64(42); answer @ (0) end" with
  | exception Elaborate.ElabError (Elaborate.UnboundVariable "stx_make_i64") -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected direct stx_* primitive name to be hidden"

let test_syntax_module_application_builder () =
  check_i64_macro "Syntax.ap builder" 3L
    "do
       macro add(_) -> Syntax.ap(Syntax.ap(Syntax.var(\"+\"), Syntax.i64(1)), Syntax.i64(2))
       add @ (0)
     end" ()

let test_syntax_module_literal_builders () =
  check_i64_macro "Syntax char/unit builders" 42L
    "do
       macro char_a(_) -> Syntax.char('a')
       macro unit_value(_) -> Syntax.unit(())
       if char_a @ (0) == 'a' do
         if unit_value @ (0) == () do 42 else 0 end
       else 0 end
      end" ()

let test_syntax_module_let_builder () =
  check_i64_macro "Syntax let builder" 7L
    "do
       macro answer(_) -> Syntax.let_in(\"x\", Syntax.i64(3), Syntax.ap(Syntax.ap(Syntax.var(\"+\"), Syntax.var(\"x\")), Syntax.i64(4)))
       answer @ (0)
      end" ()

let test_operator_macro_discards_unelaborated_perform_operand () =
  check_i64_macro "operator macro discards perform operand before elaboration" 7L
    "do
       syntax discard do | discard $x -> 7 end
       discard perform Missing.get(())
      end" ()

let test_7g_adt_matching_hygiene_roundtrip () =
  check_i64_macro "7G: ADT matching preserves binding hygiene" 42L
    "do x = 1; macro passthrough(stx) -> match stx do | Syntax.Lam(_, _) -> stx | _ -> stx end; (passthrough @ (fn(x) -> x))(42) end" ()

let test_7g_adt_matching_hygiene_introduced_body () =
  check_i64_macro "7G: ADT destructured body not captured by outer scope" 80L
    "do macro double(stx) -> match stx do | Syntax.Lam(_, body) -> Syntax.lam(\"x\", Syntax.ap(Syntax.ap(Syntax.var(\"+\"), body), body)) | _ -> Syntax.i64(0) end; (double @ (fn(x) -> x))(40) end" ()

let test_7g_adt_matching_flip_args () =
  check_i64_macro "7G: computed multi-kind dispatch not possible with templates" 1L
    "do macro classify(stx) -> match stx do | Syntax.Lam(_, _) -> Syntax.i64(1) | Syntax.Ap(_, _) -> Syntax.i64(2) | Syntax.Var(_) -> Syntax.i64(3) | _ -> Syntax.i64(0) end; classify @ (fn(x) -> x) end" ()

let test_7g_adt_simple_flip () =
  check_i64_macro "7G: simple swap args via nested match" (-2L)
    "do macro swap(stx) -> match stx do | Syntax.Ap(inner, b) -> match inner do | Syntax.Ap(f, a) -> Syntax.ap(Syntax.ap(f, b), a) | _ -> stx end | _ -> stx end; result = swap @ ((fn(x, y) -> x - y)(5, 3)); result end" ()

let test_7g_diag_outer_binders () =
  check_i64_macro "7G: DEBUG outer match with named binders" 1L
    "do macro t(stx) -> match stx do | Syntax.Ap(inner, b) -> Syntax.i64(1) | _ -> Syntax.i64(0) end; result = t @ ((fn(x, y) -> x - y)(5, 3)); result end" ()

let test_7g_diag_inner_binders_wildcards () =
  check_i64_macro "7G: DEBUG inner match with wildcards only" 1L
    "do macro t(stx) -> match stx do | Syntax.Ap(inner, _) -> match inner do | Syntax.Ap(_, _) -> Syntax.i64(1) | _ -> Syntax.i64(0) end | _ -> Syntax.i64(0) end; result = t @ ((fn(x, y) -> x - y)(5, 3)); result end" ()

let test_7g_diag_inner_binders_named () =
  check_i64_macro "7G: DEBUG inner match with named binders" 1L
    "do macro t(stx) -> match stx do | Syntax.Ap(inner, _) -> match inner do | Syntax.Ap(f, a) -> Syntax.i64(1) | _ -> Syntax.i64(0) end | _ -> Syntax.i64(0) end; result = t @ ((fn(x, y) -> x - y)(5, 3)); result end" ()

let test_7i_generated_syntax_later_wins_shadow () =
  match
    eval_with_imported_macros
      [ ("gen", "syntax build_inc do\n\
                 | build_inc ->\n\
                     multi\n\
                       syntax inc do | inc $x -> $x + 1 end\n\
                     end\n\
                 end;\n\
                 build_inc;\n\
                 syntax inc do | inc $x -> $x + 100 end;\n\
                 pub result = inc 1") ]
      "do M = import \"gen\"; M.result end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated syntax later-wins" 101L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I later-wins: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I later-wins: %s" (Printexc.to_string e))

let test_imported_operator_prefix_expands () =
  match
    eval_with_imported_macros
      [ ("ops", "pub syntax answer do | answer -> 42 end\npub x = 1") ]
      "do
         Ops = import \"ops\"
          answer
        end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "imported operator prefix" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "imported operator prefix: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "imported operator prefix: %s" (Printexc.to_string e))

let test_imported_syntax_not_runtime_field () =
  match
    eval_with_imported_macros
      [ ("ops", "pub syntax answer do | answer -> 42 end\npub x = 1") ]
      "do Ops = import \"ops\"; Ops.answer end"
  with
  | exception Elaborate.ElabError _ -> ()
  | exception Nbe.EvalError _ -> ()
  | exception e -> Alcotest.fail (Printf.sprintf "unexpected exception: %s" (Printexc.to_string e))
  | _ -> Alcotest.fail "expected imported syntax extension to be compile-time only"

let test_operator_prefix_shadowing_is_lexical () =
  check_i64_macro "operator shadowing" 1L
    "do
       syntax choose do | choose -> 1 end
       ignored = do
         syntax choose do | choose -> 2 end
         choose
       end
       choose
      end" ()

let test_syntax_template_unless () =
  check_i64_macro "unless false passes through" 10L
    "do
       syntax unless do
       | unless $cond $branch -> if $cond do 0 else $branch end
       end
       unless false 10
     end" ()

let test_syntax_template_when_match () =
  check_i64_macro "when false falls back" 0L
    "do
       syntax when do
       | when $cond $branch else $fallback ->
           if $cond do $branch else $fallback end
       end
       when false 42 else 0
     end" ()

let test_syntax_template_hole_reuse () =
  check_i64_macro "twice hole reuse" 6L
    "do
       syntax twice do | twice $x -> $x + $x end
       twice 3
     end" ()

let test_syntax_template_do_end_delimiters () =
  check_i64_macro "extract expression from do block" 42L
    "do
       syntax extract do
       | extract do $body end -> $body
       end
       extract do 42 end
     end" ()

let test_syntax_template_hygiene () =
  check_i64_macro "hygiene: template binder does not capture use-site" 1L
    "do
       x = 1
       syntax let_in do
       | let_in $val $body -> do x = $val; $body end
       end
       let_in 2 x
     end" ()

let test_syntax_template_def_site_scope () =
  check_i64_macro "definition-site scope for template refs" 2L
    "do
       y = 2
       syntax get_y do | get_y -> y end
       do
         y = 99
         get_y
       end
     end" ()

let test_syntax_template_hole_keeps_use_site_scope () =
  check_i64_macro "captured hole keeps use-site scope" 99L
    "do
       x = 1
       syntax passthrough do | passthrough $body -> $body end
       do
         x = 99
         passthrough x
       end
     end" ()

let test_syntax_template_intro_binding_captures_intro_ref () =
  check_i64_macro "introduced binder captures introduced reference" 7L
    "do
       x = 1
       syntax local_x do | local_x -> do x = 7; x end end
       do
         x = 99
         local_x
       end
     end" ()

let test_syntax_template_def_site_scope_through_lambda () =
  check_i64_macro "definition-site ref is not captured by lambda use site" 1L
    "do
       x = 1
       syntax get_x do | get_x -> x end
       (fn(x) -> get_x)(99)
     end" ()

let test_syntax_template_nested_def_site_scope () =
  check_i64_macro "definition-site ref ignores nested use-site shadows" 3L
    "do
       z = 3
       syntax get_z do | get_z -> z end
       do
         z = 4
         do
           z = 5
           get_z
         end
       end
     end" ()

let test_syntax_template_generates_syntax_form () =
  check_i64_macro "generated syntax forms are usable in generated body" 42L
    "do
       syntax build_choose do
       | build_choose -> do
           syntax flag do
           | flag yes -> true
           | flag no -> false
           end
           syntax choose do
           | choose $cond then $branch else $fallback ->
               if $cond do $branch else $fallback end
           end
           choose flag yes then 40 + 2 else 0
         end
       end
       build_choose
     end" ()

let test_syntax_template_generated_syntax_closes_over_hole () =
  check_i64_macro "generated syntax form can reuse outer hole" 16L
    "do
       syntax make_adder do
       | make_adder $base -> do
           syntax add_base do | add_base $x -> $x + $base end
           add_base 5 + add_base 1
         end
       end
       make_adder 5
     end" ()

let test_syntax_template_generated_syntax_scope_is_local () =
  check_i64_macro "generated syntax form does not shadow caller syntax" 6L
    "do
       syntax tag do | tag $x -> $x + 1 end
       syntax make_tag do
       | make_tag -> do
           syntax tag do | tag $x -> $x + 2 end
           tag 2
         end
       end
       make_tag + tag 1
     end" ()

let test_syntax_template_expands_to_template_use () =
  check_i64_macro "syntax template can expand to another syntax use" 5L
    "do
       syntax five do | five -> 5 end
       syntax call_five do | call_five -> five end
       call_five
     end" ()

let test_syntax_template_generates_parameterized_syntax_form () =
  check_i64_macro "generated syntax form can bind its own holes and branches" 11L
    "do
       syntax make_bounded do
       | make_bounded $limit -> do
           syntax bound do
           | bound $x below -> if $x < $limit do $x else $limit end
           | bound $x above -> if $x > $limit do $x else $limit end
           end
           bound 2 below + bound 9 above
         end
       end
       make_bounded 5
     end" ()

let test_syntax_template_nested_callsite_parenthesized () =
  check_i64_macro "nested syntax callsite inside parenthesized holes" 4L
    "do
       syntax is_zero do | is_zero $x -> $x == 0 end
       syntax inc do | inc $x -> $x + 1 end
       syntax wrap do | wrap $x -> $x + 1 end
       syntax choose do
       | choose $cond then $branch else $fallback ->
           if $cond do $branch else $fallback end
       end
       choose (is_zero 0) then (wrap (inc 2)) else (wrap 10)
     end" ()

let test_syntax_template_nested_callsite_unparenthesized () =
  check_i64_macro "nested syntax callsite inside unparenthesized holes" 7L
    "do
       syntax bool do
       | bool yes -> true
       | bool no -> false
       end
       syntax add2 do | add2 $x -> $x + 2 end
       syntax pick do
       | pick $cond then $branch otherwise $fallback ->
           if $cond do $branch else $fallback end
       end
       pick bool yes then add2 5 otherwise add2 10
     end" ()

let test_syntax_template_nested_callsite_repeated_hole () =
  check_i64_macro "nested syntax callsite in repeated hole" 12L
    "do
       syntax inc do | inc $x -> $x + 1 end
       syntax triple do | triple $x -> $x + $x + $x end
       triple (inc (inc 2))
     end" ()

let test_syntax_template_multi_branch () =
  check_i64_macro "multi-branch: first-match disambiguation" 1L
    "do
       syntax choose do
       | choose one -> 1
       | choose two -> 2
       end
       choose one
     end" ()

let test_syntax_template_imported_pub_syntax () =
  match
    eval_with_imported_macros
      [ ("syntax_lib",
         "pub syntax inc do | inc $x -> $x + 1 end
          pub x = 0") ]
      "do
         M = import \"syntax_lib\"
         inc 3
       end"
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
         "pub syntax local_x do | local_x -> do x = 7; x end end
          pub x = 0") ]
      "do
         M = import \"syntax_lib\"
         x = 99
         local_x
       end"
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
    "do
       syntax answer do | answer -> 42 end
       answer
     end" ()

let test_syntax_template_binder_hole () =
  check_i64_macro "binder hole can introduce use-site name" 3L
    "do
       syntax bind do
       | bind $(name: binder) $value in $body -> do $name = $value; $body end
       end
       bind x 3 in x
     end" ()

let test_syntax_template_ident_hole () =
  check_i64_macro "identifier hole can reference use-site name" 4L
    "do
       x = 4
       syntax use do | use $(name: ident) -> $name end
       use x
     end" ()

let test_syntax_template_unused_capture () =
  check_i64_macro "unused captured hole is accepted" 7L
    "do
       syntax ignore do | ignore $unused -> 7 end
       ignore MissingName
     end" ()

let test_syntax_template_reuse_duplicates_evaluation () =
  check_i64_macro "reused expression hole duplicates evaluation" 3L
    "do
       r = ref(0)
       inc = fn(_) -> do n = deref(r); _ = r <- n + 1; deref(r) end
       syntax twice do | twice $x -> $x + $x end
       twice (inc())
     end" ()

let test_decl_template_module_captures_pub_value () =
  check_i64_macro "decl template captures public module value" 42L
    "do
       M = module
         syntax keep do | keep $(d: decl) -> $d end
         keep pub answer = 42
       end;
       M.answer
     end" ()

let test_decl_template_module_preserves_typed_value () =
  check_i64_macro "decl template preserves typed value" 42L
    "do
       M = module
         syntax keep do | keep $(d: decl) -> $d end
         keep pub answer : I64 = 42
       end;
       M.answer
     end" ()

let test_decl_template_struct_captures_pub_value () =
  check_i64_macro "decl template captures public struct value" 42L
    "do
       Box = struct
         value: I64
         syntax keep do | keep $(d: decl) -> $d end
         keep pub answer = 42
       end;
       Box.answer
     end" ()

let test_decl_template_multi_generates_siblings () =
  match
    eval_with_imported_macros
      [ ( "decls",
          "syntax pair do
           | pair -> multi
               base = 40;
               pub answer = base + 2
             end
           end;
           pair" ) ]
      "do M = import \"decls\"; M.answer end"
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
          "pub syntax bad do
           | bad -> multi
               x = 1
             end
           end" ) ]
      "do M = import \"bad_syntax\"; bad end"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions declaration context" true
        (string_contains msg "declaration syntax templates")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected multi expression-template rejection"

let test_decl_template_struct_field_deferred () =
  match
    eval_with_macros
      "do
         Box = struct
           syntax field do | field -> value: I64 end
           field
         end;
         0
       end"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions deferred struct fields" true
        (string_contains msg "struct field declarations are deferred")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected struct field declaration-template rejection"

let test_7i_generated_pub_syntax_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "syntax export_syntax do
                  | export_syntax ->
                      multi
                        pub syntax inc do | inc $x -> $x + 1 end
                      end
                  end;
                  export_syntax") ]
      "do M = import \"gen\"; inc 41 end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub syntax across imports" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub syntax: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub syntax: %s" (Printexc.to_string e))

let test_7i_generated_syntax_usable_later_same_module () =
  check_import_i64 "7I generated syntax usable later" 
    [ ("gen", "syntax make_inc do
               | make_inc ->
                   multi
                     syntax inc do | inc $x -> $x + 1 end
                   end
               end;
               make_inc;
               pub result = inc 5") ]
    6L "do M = import \"gen\"; M.result end" ()

let test_7i_generated_pub_operator_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "syntax export_operator do
                  | export_operator ->
                      multi
                        pub infix (~) 15 Left (stx) -> Syntax.i64(9)
                      end
                  end;
                  export_operator") ]
      "do M = import \"gen\"; 1 ~ 2 end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub operator across imports" 9L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub operator: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub operator: %s" (Printexc.to_string e))

let test_7i_generated_pub_macro_across_imports () =
  match
    eval_with_imported_macros
      [ ("gen", "syntax export_macro do
                  | export_macro ->
                      multi
                        pub macro answer(_) -> Syntax.i64(42)
                      end
                  end;
                  export_macro") ]
      "do M = import \"gen\"; answer @ (0) end"
  with
  | VAtom (I64 n) -> Alcotest.(check int64) "7I generated pub macro across imports" 42L n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "7I generated pub macro: %s" (Debug.pp_value_short mc v))
  | exception e -> Alcotest.fail (Printf.sprintf "7I generated pub macro: %s" (Printexc.to_string e))

let test_7i_generated_pub_operator_rejected_in_struct () =
  match
    eval_with_macros
      "struct
           syntax export_operator do
           | export_operator ->
               multi
                 pub infix (~) 15 Left (stx) -> Syntax.i64(9)
               end
           end;
           export_operator
         end"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions pub operator in struct" true
        (string_contains msg "pub operator is not supported inside structs")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated pub operator in struct to be rejected"

let test_7i_generated_pub_macro_rejected_in_struct () =
  match
    eval_with_macros
      "struct
           syntax export_macro do
           | export_macro ->
               multi
                 pub macro answer(_) -> Syntax.i64(42)
               end
           end;
           export_macro
         end"
  with
  | exception Enforest.Error msg ->
      Alcotest.(check bool) "mentions pub macro in struct" true
        (string_contains msg "pub macro is not supported inside structs")
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated pub macro in struct to be rejected"

let test_7i_generated_syntax_cycle () =
  match
    eval_with_imported_macros
      [ ("cycle_a", "syntax gen_aop do
                     | gen_aop -> multi
                         pub syntax aop do | aop $x -> do import \"cycle_b\"; $x end end
                       end
                     end;
                     gen_aop");
        ("cycle_b", "syntax gen_bop do
                     | gen_bop -> multi
                         pub syntax bop do | bop $x -> do import \"cycle_a\"; $x end end
                       end
                     end;
                     gen_bop")
      ] "do A = import \"cycle_a\"; aop 0 end"
  with
  | exception Core_loader.CircularSyntaxVisit "cycle_a" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated syntax circular visit"

let test_7i_generated_macro_cycle () =
  match
    eval_with_imported_macros
      [ ("mac_a", "syntax gen_ma do
                   | gen_ma -> multi
                       pub macro ma(_) -> do B = import \"mac_b\"; Syntax.i64(1) end
                     end
                   end;
                   gen_ma");
        ("mac_b", "syntax gen_mb do
                   | gen_mb -> multi
                       pub macro mb(_) -> do A = import \"mac_a\"; Syntax.i64(2) end
                     end
                   end;
                   gen_mb")
      ] "do A = import \"mac_a\"; ma @ (0) end"
  with
  | exception Core_loader.CircularSyntaxVisit "mac_a" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected generated macro circular visit"

let test_7i_generated_syntax_hygiene_introduced_binder () =
  check_i64_macro "7I generated syntax hygiene preserves introduced binders" 99L
    "do
       M = module
         syntax let_x do | let_x $body -> do x = 1; $body end end;
         pub result = do
           x = 99;
           let_x x
         end
       end;
       M.result
     end" ()

let () =
  Alcotest.run "core"
    [
      ( "eval",
        [
          Alcotest.test_case "atom" `Quick (check_i64 "atom" 42L "42");
          Alcotest.test_case "lam+ap" `Quick (check_i64 "lam+ap" 7L "(fn(x) -> x)(7)");
          Alcotest.test_case "apply twice" `Quick
            (check_i64 "apply twice" 5L
               "do
                  twice : (I64 -> I64) -> I64 -> I64 = fn(f) -> fn(x) -> f(f(x))
                  inc : I64 -> I64 = fn(n) -> n + 1
                  twice(inc, 3)
                end");
          Alcotest.test_case "let" `Quick (check_i64 "let" 5L "do x : I64 = 5; x end");
          Alcotest.test_case "let shadowing" `Quick
            (check_i64 "let shadowing" 2L "do x = 1; x = 2; x end");
          Alcotest.test_case "non-rec let rhs sees outer" `Quick
            (check_i64 "non-rec let rhs sees outer" 1L "do x = 1; x = x; x end");
          Alcotest.test_case "lambda shadows outer let" `Quick
            (check_i64 "lambda shadows outer let" 7L "do x = 1; (fn(x) -> x : I64 -> I64)(7) end");
          Alcotest.test_case "if true" `Quick (check_i64 "if true" 1L "if true do 1 else 2 end");
          Alcotest.test_case "if false" `Quick (check_i64 "if false" 2L "if false do 1 else 2 end");
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "proj" `Quick (check_i64 "proj" 42L "(42, true).0");
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "module signature argument" `Quick test_eval_module_signature_argument;
          Alcotest.test_case "module signature extra field" `Quick test_eval_module_signature_extra_field;
          Alcotest.test_case "signature sugar argument" `Quick test_eval_signature_sugar_argument;
          Alcotest.test_case "module signature functor" `Quick test_eval_module_signature_functor;
          Alcotest.test_case "ref read initial" `Quick test_ref_read_initial;
          Alcotest.test_case "ref write read" `Quick test_ref_write_read;
          Alcotest.test_case "ref aliases share cell" `Quick test_ref_aliases_share_cell;
          Alcotest.test_case "ref closure observes later write" `Quick test_ref_closure_observes_later_write;
          Alcotest.test_case "ref repeated closure increments" `Quick test_ref_repeated_closure_increments;
          Alcotest.test_case "pi" `Quick test_eval_pi;
          Alcotest.test_case "eq i64" `Quick (check_bool "eq i64" true "1 == 1");
          Alcotest.test_case "neq i64" `Quick (check_bool "neq i64" true "1 != 2");
          Alcotest.test_case "eq bool" `Quick (check_bool "eq bool" false "true == false");
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
               "do rec f : Bool -> I64 = fn(x) -> if x do 0 else f(true) end; f(false) end");
          Alcotest.test_case "rec sum" `Quick
            (check_i64 "rec sum" 15L
               "do rec sum : I64 -> I64 = fn(n) -> if n == 0 do 0 else sum(n - 1) + n end; sum(5) end");
          Alcotest.test_case "factorial" `Quick
            (check_i64 "factorial" 120L
               "do rec fact : I64 -> I64 = fn(n) -> if n == 0 do 1 else n * fact(n - 1) end; fact(5) end");
          Alcotest.test_case "fibonacci" `Quick
            (check_i64 "fibonacci" 8L
               "do rec fib : I64 -> I64 = fn(n) -> if n <= 1 do n else fib(n - 1) + fib(n - 2) end; fib(6) end");
          Alcotest.test_case "rec count" `Quick
            (check_i64 "rec count" 5L
               "do rec f : I64 -> I64 = fn(n) -> if n == 5 do 5 else f(n + 1) end; f(0) end");
          Alcotest.test_case "rec not" `Quick
            (check_i64 "rec not" 0L
               "do rec f : Bool -> I64 = fn(x) -> if x do 0 else f(not x) end; f(false) end");
          Alcotest.test_case "unhandled perform" `Quick test_eval_unhandled_perform;
          Alcotest.test_case "handler ignores continuation" `Quick test_eval_handler_ignores_continuation;
          Alcotest.test_case "handler resumes once" `Quick test_eval_handler_resumes_once;
          Alcotest.test_case "handler value branch" `Quick test_eval_handler_value_branch;
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
          Alcotest.test_case "type-case Bool false" `Quick test_eval_type_case_bool_false;
          Alcotest.test_case "type-case Bool true" `Quick test_eval_type_case_bool_true;
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
               "do type Color = Red | Green; match Red do Red -> 1 | Green -> 2 end end");
          Alcotest.test_case "match wildcard" `Quick
            (check_i64 "match wildcard" 99L
               "do type Color = Red | Green | Blue; \
                match Green do Red -> 1 | _ -> 99 end end");
           Alcotest.test_case "match bind" `Quick
             (check_i64 "match bind" 42L
                "do type Option a = Some(a) | None; \
                  match Some(42) do Some(x) -> x | None -> 0 end end");
          Alcotest.test_case "match binder shadows outer" `Quick
            (check_i64 "match binder shadows outer" 7L
               "do x = 99; match 7 do x -> x end end");
           Alcotest.test_case "match constructor or-pattern binding" `Quick
             (check_i64 "match constructor or-pattern binding" 5L
                "do type E = A(I64) | B(I64); \
                 match B(5) do (A(x) | B(x)) -> x end end");
           Alcotest.test_case "match nested" `Quick
             (check_i64 "match nested" 7L
                "do type Option a = Some(a) | None; \
                 match Some(Some(7)) do \
                   | Some(Some(x)) -> x | Some(None) -> 0 | None -> 0 end end");
             Alcotest.test_case "recursive parameterized ADT match" `Quick
               (check_i64 "recursive parameterized ADT match" 1L
                  "do type List(a) = Cons(a, List(a)) | Nil; \
                   match Cons(1, Nil) do Cons(x, _) -> x | Nil -> 0 end end");
             Alcotest.test_case "recursive list sum" `Quick
               (check_i64 "recursive list sum" 6L
                  "do type List(a) = Cons(a, List(a)) | Nil; \
                   rec sum : List(I64) -> I64 = fn(xs) do \
                     match xs do Cons(x, rest) -> x + sum(rest) | Nil -> 0 end end; \
                   sum(Cons(1, Cons(2, Cons(3, Nil)))) end");
             Alcotest.test_case "constructor comma payload distinct from tuple" `Quick
               (check_i64 "constructor comma payload distinct from tuple" 6L
                  "do type Tuple(a, b, c) = T(a, b * c); \
                   match T(1, (2, 3)) do T(x, yz) -> x + yz.0 + yz.1 end end");
             Alcotest.test_case "constructor tuple payload remains single arg" `Quick
               (check_i64 "constructor tuple payload remains single arg" 1L
                  "do type Pair = P(I64 * Bool); \
                   match P((1, true)) do P(pair) -> pair.0 end end");
           Alcotest.test_case "qualified constructor pattern" `Quick
             (check_i64 "qualified constructor pattern" 2L
                "do S = module pub type Color = Red | Green end; \
                 open S; match Green do Red -> 1 | Green -> 2 end end");
           Alcotest.test_case "qualified nested constructor pattern" `Quick
             (check_i64 "qualified nested constructor pattern" 7L
                "do A = module pub B = module pub type T = X(I64) | Y end end; \
                 open A; open B; match X(7) do X(n) -> n | Y -> 0 end end");
           Alcotest.test_case "qualified constructor alias pattern" `Quick
             (check_i64 "qualified constructor alias pattern" 1L
                "do S = module pub type Color = Red | Green end; \
                 N = S; open N; match Red do Red -> 1 | Green -> 2 end end");
          Alcotest.test_case "match int literal hit" `Quick
            (check_i64 "match int literal hit" 10L
               "match 1 do 1 -> 10 | _ -> 20 end");
          Alcotest.test_case "match int literal default" `Quick
            (check_i64 "match int literal default" 20L
               "match 2 do 1 -> 10 | _ -> 20 end");
           Alcotest.test_case "match literal or-pattern" `Quick
             (check_i64 "match literal or-pattern" 42L
                "match 1 do (0 | 1) -> 42 | _ -> 0 end");
          Alcotest.test_case "match bool literal" `Quick
            (check_i64 "match bool literal" 0L
               "match false do true -> 1 | false -> 0 end");
          Alcotest.test_case "match unit literal" `Quick
            (check_i64 "match unit literal" 7L
               "match () do () -> 7 end");
          Alcotest.test_case "match char literal hit" `Quick
            (check_i64 "match char literal hit" 10L
               "match 'a' do 'a' -> 10 | _ -> 20 end");
          Alcotest.test_case "match char literal default" `Quick
            (check_i64 "match char literal default" 20L
               "match 'b' do 'a' -> 10 | _ -> 20 end");
          Alcotest.test_case "match escaped char literal" `Quick
            (check_i64 "match escaped char literal" 1L
               "match '\\n' do '\\n' -> 1 | _ -> 0 end");
          Alcotest.test_case "match literal binder fallback" `Quick
            (check_i64 "match literal binder fallback" 42L
               "match 42 do 0 -> 0 | x -> x end");
          Alcotest.test_case "match first branch wins" `Quick
            (check_i64 "match first branch wins" 0L
               "match 1 do _ -> 0 | 1 -> 1 end");
           Alcotest.test_case "match tagged payload" `Quick
             (check_i64 "match tagged payload" 42L
                "do type Wrapper = W(I64); match W(41) do W(x) -> x + 1 end end");
           Alcotest.test_case "match tuple bind" `Quick
             (check_i64 "match tuple bind" 1L
                "match (1, true) do (x, b) -> if b do x else 0 end end");
          Alcotest.test_case "match tuple wildcard" `Quick
            (check_i64 "match tuple wildcard" 2L
               "match (1, 2) do (_, y) -> y end");
          Alcotest.test_case "match whole tuple binder" `Quick
            (check_i64 "match whole tuple binder" 1L
               "match (1, true) do p -> p.0 end");
          Alcotest.test_case "match nested tuple" `Quick
            (check_i64 "match nested tuple" 3L
               "match ((1, true), 2) do ((x, _), y) -> x + y end");
          Alcotest.test_case "match tuple literals" `Quick
            (check_i64 "match tuple literals" 9L
               "match (false, 1) do (true, x) -> x | (false, _) -> 9 end");
          Alcotest.test_case "record field access" `Quick
            (check_i64 "record field access" 1L
               "do Point = struct x: I64; y: I64; end; (Point{x = 1; y = 2}).x end");
           Alcotest.test_case "parameterized record construction" `Quick
             (check_bool "parameterized record construction" true
                "do Pair = fn[A : Type, B : Type] -> struct fst: A; snd: B; end; (Pair[I64, Bool]{fst = 1; snd = true}).snd end");
          Alcotest.test_case "record type declaration" `Quick
            (check_i64 "record type declaration" 2L
               "do type Point = {x: I64; y: I64}; (Point{x = 1; y = 2}).y end");
          Alcotest.test_case "record construction field order" `Quick
            (check_i64 "record construction field order" 30L
               "do type Point = {x: I64; y: I64}; \
                p = Point{y = 20; x = 10}; p.x + p.y end");
          Alcotest.test_case "parameterized record type declaration" `Quick
            (check_bool "parameterized record type declaration" true
               "do type Pair A B = {fst: A; snd: B}; (Pair{fst = 1; snd = true}).snd end");
           Alcotest.test_case "polymorphic record multiple instantiations" `Quick
             (check_i64 "polymorphic record multiple instantiations" 13L
                "do type Pair A B = {fst: A; snd: B}; \
                 p1 = Pair{fst = 10; snd = 20}; \
                 p2 = Pair{fst = true; snd = 3}; \
                 if p2.fst do p1.fst + p2.snd else 0 end end");
            Alcotest.test_case "parameterized record method" `Quick
              (check_bool "parameterized record method" true
                 "do Pair = fn[A : Type, B : Type] -> struct fst: A; snd: B; pub swap = fn(p) -> (p.snd, p.fst) end; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = true})).0 end");
            Alcotest.test_case "method uses self" `Quick
              (check_i64 "method uses self" 1L
                 "do Box = fn[A : Type] -> struct value: A; pub method get() do self.value end end; Box[I64].get(Box[I64]{value = 1}) end");
            Alcotest.test_case "parameterized method uses self" `Quick
              (check_bool "parameterized method uses self" true
                 "do Pair = fn[A : Type, B : Type] -> struct fst: A; snd: B; pub method swap() do (self.snd, self.fst) end end; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = true})).0 end");
           Alcotest.test_case "method extra parameter" `Quick
             (check_i64 "method extra parameter" 3L
                "do Counter = struct value: I64; pub method add(x) do self.value + x end end; Counter.add(Counter{value = 1})(2) end");
            Alcotest.test_case "method uses Self type" `Quick
              (check_i64 "method uses Self type" 2L
                 "do Box = fn[A : Type] -> struct value: A; pub method id(other : Self) -> other.value end; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) end");
           Alcotest.test_case "method returns Self" `Quick
             (check_i64 "method returns Self" 1L
                "do Box = struct value: I64; pub method copy() do self end end; (Box.copy(Box{value = 1})).value end");
           Alcotest.test_case "struct impl for Self" `Quick
             (check_bool "struct impl for Self" true
                 "do Point = struct \
                    x: I64; \
                    pub impl Eq(Self) = module fn eq(lhs, rhs) -> lhs.x == rhs.x end \
                  end; \
                  Point{x = 1} == Point{x = 1} end");
          Alcotest.test_case "record pattern shorthand" `Quick
            (check_i64 "record pattern shorthand" 3L
               "do Point = struct x: I64; y: I64; end; \
                match Point{x = 1; y = 2} do Point {x; y} -> x + y end end");
          Alcotest.test_case "record pattern reordered" `Quick
            (check_i64 "record pattern reordered" 3L
               "do Point = struct x: I64; y: I64; end; \
                match Point{x = 1; y = 2} do Point {y; x} -> x + y end end");
          Alcotest.test_case "record pattern renamed field" `Quick
            (check_i64 "record pattern renamed field" 30L
               "do type Point = {x: I64; y: I64}; \
                match Point{x = 10; y = 20} do Point {x = wow; y} -> wow + y end end");
          Alcotest.test_case "record pattern partial" `Quick
            (check_i64 "record pattern partial" 3L
               "do type Point = {x: I64; y: I64}; \
                match Point{x = 3; y = 4} do Point {x; _} -> x end end");
          Alcotest.test_case "record pattern literal dispatch" `Quick
            (check_i64 "record pattern literal dispatch" 4L
               "do Flag = struct flag: Bool; value: I64; end; \
                match Flag{flag = false; value = 3} do \
                | Flag {flag = true; value} -> value | Flag {flag = false; value} -> value + 1 end end");
           Alcotest.test_case "qualified record pattern" `Quick
             (check_i64 "qualified record pattern" 3L
                "do M = module pub Point = struct x: I64; y: I64; end end; \
                 open M; match Point{x = 1; y = 2} do Point {x; y} -> x + y end end");
           Alcotest.test_case "struct private helper" `Quick
             (check_i64 "struct private helper" 11L
                "do M = module secret = 10; pub x = secret + 1 end; M.x end");
           Alcotest.test_case "module public function" `Quick
             (check_i64 "module public function" 42L
                "do M = module helper = fn(x) -> x * 2; pub double = helper end; M.double(21) end");
           Alcotest.test_case "struct multiple public members" `Quick
             (check_i64 "struct multiple public members" 3L
                "do M = module pub a = 1; pub b = 2 end; M.a + M.b end");
           Alcotest.test_case "open struct values" `Quick
             (check_i64 "open struct values" 52L
                "do M = module pub x = 42; pub y = 10 end; open M; x + y end");
          Alcotest.test_case "open struct constructors" `Quick
            (check_i64 "open struct constructors" 1L
               "do Color = module pub type Color = Red | Green | Blue end; \
                open Color; match Red do Red -> 1 | Green -> 2 | Blue -> 3 end end");
        ] );
      ( "imports",
        [
            Alcotest.test_case "basic import" `Quick
              (check_import_i64 "basic import" [ ("math", "pub x = 41; pub y = x + 1") ] 42L
                 "do M = import \"math\"; M.y end");
            Alcotest.test_case "imported public function" `Quick
              (check_import_i64 "imported public function" [ ("math", "pub fn double(x) -> x + x") ] 10L
                 "do M = import \"math\"; M.double(5) end");
            Alcotest.test_case "nested import" `Quick
              (check_import_i64 "nested import"
                 [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ] 42L
                 "do W = import \"wrapper\"; W.M.x end");
            Alcotest.test_case "open imported module exposes public value" `Quick
              (check_import_i64 "open imported module exposes public value" [ ("math", "pub x = 42") ] 42L
                 "do M = import \"math\"; open M; x end");
            Alcotest.test_case "open imported nested module" `Quick
              (check_import_i64 "open imported nested module"
                 [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ] 42L
                 "do W = import \"wrapper\"; open W; open M; x end");
            Alcotest.test_case "open imported module local only" `Quick
              (check_import_i64 "open imported module local only"
                 [ ("base", "pub x = 41"); ("wrapper", "B = import \"base\"; pub y = do open B; x + 1 end") ]
                 42L
                 "do W = import \"wrapper\"; W.y end");
            Alcotest.test_case "repeated import" `Quick
              (check_import_i64 "repeated import" [ ("m", "pub x = 21") ] 42L
                 "do A = import \"m\"; B = import \"m\"; A.x + B.x end");
            Alcotest.test_case "imported ADT match" `Quick
              (check_import_i64 "imported ADT match"
                 [ ("color", "pub type Color = Red | Green | Blue; pub default = Green") ] 2L
                 "do C = import \"color\"; match C.default do C.Red -> 1 | C.Green -> 2 | C.Blue -> 3 end end");
            Alcotest.test_case "open imported module exposes constructors" `Quick
              (check_import_i64 "open imported module exposes constructors"
                 [ ("color", "pub type Color = Red | Green") ] 1L
                 "do C = import \"color\"; open C; match Red do Red -> 1 | Green -> 2 end end");
            Alcotest.test_case "imported record field access" `Quick
              (check_import_i64 "imported record field access"
                 [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 1L
                 "do S = import \"shapes\"; (S.Point{x = 1; y = 2}).x end");
            Alcotest.test_case "imported record pattern" `Quick
              (check_import_i64 "imported record pattern"
                 [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 3L
                 "do S = import \"shapes\"; match S.Point{x = 1; y = 2} do S.Point {x; y} -> x + y end end");
            Alcotest.test_case "imported record pattern alias" `Quick
              (check_import_i64 "imported record pattern alias"
                 [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 3L
                 "do S = import \"shapes\"; Alias = S; match S.Point{x = 1; y = 2} do Alias.Point {x; y} -> x + y end end");
            Alcotest.test_case "imported nested constructor pattern" `Quick
              (check_import_i64 "imported nested constructor pattern"
                 [ ("nested", "pub module M do pub type T = X(I64) | Y end") ] 7L
                 "do N = import \"nested\"; match N.M.X(7) do N.M.X(n) -> n | N.M.Y -> 0 end end");
            Alcotest.test_case "imported module alias pattern" `Quick
              (check_import_i64 "imported module alias pattern"
                 [ ("color", "pub type Color = Red | Green") ] 1L
                 "do C = import \"color\"; Alias = C; match C.Red do Alias.Red -> 1 | Alias.Green -> 2 end end");
            Alcotest.test_case "imported public effect handler" `Quick
              (check_import_i64 "imported public effect handler"
                 [ ("effects", "pub effect Exc = sig raise : I64 -> I64 end") ] 2L
                 "do E = import \"effects\"; match perform E.Exc.raise(1) do x -> x | effect E.Exc.raise n -> n + 1 end end");
            Alcotest.test_case "open imported module exposes effect" `Quick
              (check_import_i64 "open imported module exposes effect"
                 [ ("effects", "pub effect Exc = sig raise : I64 -> I64 end") ] 2L
                 "do E = import \"effects\"; open E; match perform Exc.raise(1) do x -> x | effect Exc.raise n -> n + 1 end end");
            Alcotest.test_case "imported public parameterized effect handler" `Quick
              (check_import_i64 "imported public parameterized effect handler"
                 [ ("effects", "pub effect State(S) = sig get : Unit -> S end") ] 42L
                 "do E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  match perform StateI64.get(()) do x -> x | effect StateI64.get () -> 42 end end");
            Alcotest.test_case "imported latent effect function" `Quick
              (check_import_i64 "imported latent effect function"
                 [ ( "effects",
                     "pub effect State(S) = sig get : Unit -> S end; \
                      pub read : Unit -> I64 can State(I64) = fn(_) -> perform State.get(())" ) ]
                 7L
                 "do E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  match E.read(()) do x -> x | effect StateI64.get () -> 7 end end");
            Alcotest.test_case "imported parameterized handler distinguishes instances" `Quick
              (check_import_i64 "imported parameterized handler distinguishes instances"
                 [ ("effects", "pub effect State(S) = sig get : Unit -> S end") ] 1L
                 "do E = import \"effects\"; \
                  StateI64 = E.State(I64); \
                  StateBool = E.State(Bool); \
                  match (if perform StateBool.get(()) do perform StateI64.get(()) else 0 end) do \
                    x -> x \
                  | effect StateI64.get () -> resume(1) \
                  | effect StateBool.get () -> resume(true) \
                  end end");
        ] );
      ( "conv",
        [
           Alcotest.test_case "beta" `Quick
             (check_conv "beta" "(fn(x : I64) -> x)(5)" "5");
           Alcotest.test_case "eta" `Quick
             (check_conv "eta"
                "fn(x : I64) -> x"
                "fn(y : I64) -> y");
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
          Alcotest.test_case "imported macro not runtime field" `Quick test_imported_macro_not_runtime_field;
          Alcotest.test_case "imported macro circular visit" `Quick test_imported_macro_circular_visit;
          Alcotest.test_case "macro-generated import loads macros" `Quick test_macro_generated_import_loads_macros;
          Alcotest.test_case "macro-generated import checks missing" `Quick test_macro_generated_import_checks_missing;
          Alcotest.test_case "imported macro calls regular function" `Quick test_imported_macro_calls_regular_function;
          Alcotest.test_case "operator prefix macro expands" `Quick test_operator_prefix_macro_expands;
          Alcotest.test_case "infix macro expands" `Quick test_operator_infix_macro_expands;
          Alcotest.test_case "infix uses operands" `Quick test_operator_uses_operands;
          Alcotest.test_case "operator RHS can use earlier macro" `Quick test_operator_rhs_can_use_earlier_macro;
          Alcotest.test_case "operator prefix receives structured input" `Quick test_operator_prefix_receives_structured_input;
          Alcotest.test_case "operator macro error reports spans" `Quick test_operator_macro_error_reports_spans;
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
          Alcotest.test_case "syntax template: when/else match" `Quick test_syntax_template_when_match;
          Alcotest.test_case "syntax template: hole reuse duplicates effects" `Quick test_syntax_template_hole_reuse;
          Alcotest.test_case "syntax template: do/end delimiters" `Quick test_syntax_template_do_end_delimiters;
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
    ]
