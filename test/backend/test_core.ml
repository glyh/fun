open Core
open Atom
open Nbe
open Unify

let () =
  Printexc.register_printer (function
    | Elaborate.ElabError e ->
        let open Elaborate in
        Some
          (Printf.sprintf "ElabError(%s)"
             (match e with
             | UnboundVariable n -> "UnboundVariable \"" ^ n ^ "\""
             | ApplyingNonFunction -> "ApplyingNonFunction"
             | TupleLengthMismatch -> "TupleLengthMismatch"
             | NotANominalType -> "NotANominalType"
             | UnknownConstructor n -> "UnknownConstructor \"" ^ n ^ "\""
             | PatternArityMismatch -> "PatternArityMismatch"
             | PatternBindingMismatch -> "PatternBindingMismatch"
             | UnknownRecordField n -> "UnknownRecordField \"" ^ n ^ "\""
             | DuplicateRecordField n -> "DuplicateRecordField \"" ^ n ^ "\""
             | MissingRecordField n -> "MissingRecordField \"" ^ n ^ "\""
             | NonExhaustive msg -> "NonExhaustive \"" ^ msg ^ "\""
             | InvalidRecursiveRecord msg -> "InvalidRecursiveRecord \"" ^ msg ^ "\""
             | ImportRequiresLoader path -> "ImportRequiresLoader \"" ^ path ^ "\""
             | DuplicateEffectOperation n -> "DuplicateEffectOperation \"" ^ n ^ "\""
             | ExpectedEffect -> "ExpectedEffect"
             | DuplicateEffect -> "DuplicateEffect"
	             | DuplicateEffectBranch n -> "DuplicateEffectBranch \"" ^ n ^ "\""
	          | UnknownEffectOperation n -> "UnknownEffectOperation \"" ^ n ^ "\""
	          | EffectOperationPathExpected -> "EffectOperationPathExpected"
	          | UnhandledEffects -> "UnhandledEffects"
	          | UnknownTrait n -> "UnknownTrait \"" ^ n ^ "\""
	          | UnknownTraitMethod n -> "UnknownTraitMethod \"" ^ n ^ "\""
	          | DuplicateTraitField n -> "DuplicateTraitField \"" ^ n ^ "\""
	          | MissingTraitField n -> "MissingTraitField \"" ^ n ^ "\""
	          | AmbiguousTraitImplementation n -> "AmbiguousTraitImplementation \"" ^ n ^ "\""))
    | _ -> None)

let mc () = MetaContext.create ()
let pure_effects = effect_row_closure [] empty_effect_row

let parse_expr source =
  Core_lexer.parse_expr source

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
      | exception e -> Alcotest.fail (Printf.sprintf "%s: exception %s" label (Printexc.to_string e)))

let check_i64 label expected source () =
  match eval_source source with
  | VAtom (I64 n) -> Alcotest.(check int64) label expected n
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "%s: expected VAtom I64, got %s" label
        (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail (Printf.sprintf "%s: exception %s" label (Printexc.to_string e))

let check_bool label expected source () =
  match eval_source source with
  | VAtom (Bool b) -> Alcotest.(check bool) label expected b
  | v ->
      let mc = MetaContext.create () in
      Alcotest.fail (Printf.sprintf "%s: expected VAtom Bool, got %s" label
        (Debug.pp_value_short mc v))
  | exception e ->
      Alcotest.fail (Printf.sprintf "%s: exception %s" label (Printexc.to_string e))

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
  | VPi { domain = VAtomTy TI64; _ } -> ()
  | _ -> Alcotest.fail "expected VPi"

let test_eval_dot () =
  match eval_source "let M = module pub let x = 99 end in M.x" with
  | VAtom (I64 n) -> Alcotest.(check int64) "dot" 99L n
  | _ -> Alcotest.fail "expected VAtom"

let test_eval_module_signature_argument () =
  check_i64 "module signature argument" 42L
    "(fun (m : module let x = I64 end) -> m.x) (module pub let x = 42 end)"
    ()

let test_eval_module_signature_extra_field () =
  check_i64 "module signature extra field" 42L
    "(fun (m : module let x = I64 end) -> m.x) (module pub let x = 42; pub let y = true end)"
    ()

let test_eval_signature_sugar_argument () =
  check_i64 "signature sugar argument" 42L
    "(fun (m : sig x : I64 end) -> m.x) (module pub let x = 42 end)"
    ()

let test_eval_module_signature_functor () =
  check_i64 "module signature functor" 42L
    "let F = fun (M : module let x = I64 end) -> module pub let doubled = M.x + M.x end in (F (module pub let x = 21 end)).doubled"
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
  let v1 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy TBool } } in
  let v2 = VPi { explicitness = Explicit; domain = VAtomTy TI64; effects = pure_effects; codomain = { env = []; body = AtomTy TBool } } in
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
  let v2 = VPi { explicitness = Explicit; domain = VFlex { id; spine = [] }; effects = pure_effects; codomain = { env = []; body = AtomTy TBool } } in
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
  let nom1 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy TI64 ]; constructors = [] } in
  let nom2 = VNominal { id = 99; num_params = 1; name = "Option"; params = [ VAtomTy TBool ]; constructors = [] } in
  match unify mc [] 0 nom1 nom2 with
  | exception UnifyError _ -> ()
  | () -> Alcotest.fail "expected unify error for different nominal params"

let test_unify_effect_same_id_same_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  unify mc [] 0 eff1 eff2

let test_unify_effect_same_id_different_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy TBool ]; operations = [] } in
  match unify mc [] 0 eff1 eff2 with
  | exception UnifyError _ -> ()
  | () -> Alcotest.fail "expected unify error for different effect params"

let test_unify_effect_different_ids () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 8; name = "Env"; params = [ VAtomTy TI64 ]; operations = [] } in
  match unify mc [] 0 eff1 eff2 with
  | exception UnifyError (EffectMismatch ("State", "Env")) -> ()
  | exception UnifyError _ -> Alcotest.fail "wrong unify error"
  | () -> Alcotest.fail "expected unify error for distinct effect ids"

let test_conv_effect_same_id_same_params () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  Alcotest.(check bool) "effect conv" true (conv mc 0 eff1 eff2)

let test_conv_effect_different_ids () =
  let mc = mc () in
  let eff1 = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let eff2 = VEffect { id = 8; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  Alcotest.(check bool) "effect conv" false (conv mc 0 eff1 eff2)

let effect_row_env () =
  [ VEffect { id = 1; name = "IO"; params = []; operations = [] };
    VEffect { id = 2; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } ]

let effectful_pi effects =
  VPi
    { explicitness = Explicit;
      domain = VAtomTy TI64;
      effects = { env = effect_row_env (); effects; tail = None };
      codomain = { env = []; body = AtomTy TI64 } }

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
  let text = Debug.pp_value_short mc (VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] }) in
  Alcotest.(check string) "debug effect" "effect State(I64)" text

let test_eval_unhandled_perform () =
  let state = VEffect { id = 7; name = "State"; params = [ VAtomTy TI64 ]; operations = [] } in
  let term = Perform { eff = Var 0; op = "put"; arg = Atom (I64 42L) } in
  match Nbe.eval (mc ()) [ state ] term with
  | exception Nbe.EvalError msg ->
      if not (String.contains msg 'S') then Alcotest.fail ("unexpected perform error: " ^ msg)
  | _ -> Alcotest.fail "expected unhandled perform error"

let test_debug_perform () =
  let text = Debug.pp_term (Perform { eff = EffectRef ("State", [ AtomTy TI64 ]); op = "get"; arg = Atom Unit }) in
  if not (String.contains text 'g') then Alcotest.fail ("expected perform debug output, got " ^ text)

let test_eval_handler_ignores_continuation () =
  check_i64 "handler ignores continuation" 2L
    "let Exc = effect raise : I64 -> I64 end in match perform Exc.raise 1 with x -> x | effect Exc.raise n -> n + 1 end"
    ()

let test_eval_handler_resumes_once () =
  check_i64 "handler resumes once" 2L
    "let Exc = effect raise : I64 -> I64 end in match perform Exc.raise 1 with x -> x | effect Exc.raise n -> resume (n + 1) end"
    ()

let test_eval_handler_value_branch () =
  check_i64 "handler value branch" 42L
    "let Exc = effect raise : I64 -> I64 end in match 41 with x -> x + 1 | effect Exc.raise n -> 0 end"
    ()

let test_eval_handler_outer_bubble () =
  check_i64 "handler outer bubble" 2L
    "let Exc = effect raise : I64 -> I64 end in match (match perform Exc.raise 1 with x -> x end) with x -> x | effect Exc.raise n -> n + 1 end"
    ()

let test_eval_handler_escape_skips_continuation () =
  check_i64 "handler escape skips continuation" 99L
    "let Exit = effect now : I64 -> I64 end in \
     let program : Unit -> I64 can Exit = fun _ -> \
       let _ = perform Exit.now 99 in \
       0 \
     in \
     match program () with \
       x -> x \
     | effect Exit.now value -> value \
     end"
    ()

let test_eval_handler_ping_pong_effects () =
  check_i64 "handler ping pong effects" 212L
    "let Ping = effect hit : I64 -> I64 end in \
     let Pong = effect hit : I64 -> I64 end in \
     let program : Unit -> I64 can {Ping, Pong} = fun _ -> \
       let x = perform Ping.hit 1 in \
       perform Pong.hit (x + 10) \
     in \
     match program () with \
       x -> x \
     | effect Ping.hit n -> \
         let y = perform Pong.hit (n + 1) in \
         resume y \
     | effect Pong.hit n -> \
         resume (n + 100) \
     end"
    ()

let test_eval_recursive_handler_ping_pong_effects () =
  check_i64 "recursive handler ping pong effects" 10L
    "let Ping = effect hit : I64 -> I64 end in \
     let Pong = effect hit : I64 -> I64 end in \
     let rec loop : I64 -> I64 can {Ping, Pong} = fun n -> \
       if n == 0 then \
         0 \
       else \
         let x = perform Ping.hit n in \
         let y = perform Pong.hit (n - 1) in \
         x + y + loop (n - 2) \
     in \
     match loop 4 with \
       x -> x \
     | effect Ping.hit n -> resume n \
     | effect Pong.hit n -> resume n \
     end"
    ()

let test_eval_state_handler_sequences_operations () =
  check_i64 "state handler sequences operations" 2L
    "let State S = effect get : Unit -> S; put : S -> Unit end in \
     let program : Unit -> I64 can State I64 = fun _ -> \
       let x = perform State.get () in \
       let _ = perform State.put (x + 1) in \
       perform State.get () \
     in \
     let rec run : I64 -> (Unit -> I64 can State I64) -> I64 = fun state -> fun thunk -> \
       match thunk () with \
         x -> x \
       | effect State.get () -> run state (fun _ -> resume state) \
       | effect State.put next -> run next (fun _ -> resume ()) \
       end \
     in \
     run 1 program"
    ()

let test_eval_handler_tuple_payload_pattern () =
  check_i64 "handler tuple payload pattern" 42L
    "let Console = effect log : I64 * I64 -> I64 end in \
     match perform Console.log (40, 2) with \
       x -> x \
     | effect Console.log (level, message) -> level + message \
     end"
    ()

let test_eval_handler_record_payload_pattern () =
  check_i64 "handler record payload pattern" 42L
    "let Request = struct value: I64; extra: I64; end in \
     let Ask = effect prompt : Request -> I64 end in \
     match perform Ask.prompt (Request {value = 40; extra = 2}) with \
       x -> x \
     | effect Ask.prompt Request {value; extra} -> value + extra \
     end"
    ()

let is_zeroish_source call =
  "let is_zeroish : {T : Type} -> T -> Bool = fun {T : Type} -> fun x -> \
   match T with \
   I64 -> x == 0 \
   | Bool -> x == false \
   | Unit -> true \
   | Char -> x == 'a' \
   | _ -> false \
   end \
   in " ^ call

let test_eval_type_case_i64_zero () =
  check_bool "type-case I64 zero" true (is_zeroish_source "is_zeroish 0") ()

let test_eval_type_case_i64_nonzero () =
  check_bool "type-case I64 nonzero" false (is_zeroish_source "is_zeroish 1") ()

let test_eval_type_case_bool_false () =
  check_bool "type-case Bool false" true (is_zeroish_source "is_zeroish false") ()

let test_eval_type_case_bool_true () =
  check_bool "type-case Bool true" false (is_zeroish_source "is_zeroish true") ()

let test_eval_type_case_unit () =
  check_bool "type-case Unit" true (is_zeroish_source "is_zeroish ()") ()

let test_eval_type_case_char_a () =
  check_bool "type-case Char a" true (is_zeroish_source "is_zeroish 'a'") ()

let default_source call =
  "let default : {T : Type} -> T = fun {T : Type} -> \
   match T with \
   I64 -> 0 \
   | Bool -> false \
   | Unit -> () \
   | Char -> 'a' \
   | _ -> panic \"no default\" \
   end \
   in " ^ call

let test_eval_type_case_default_i64 () =
  check_i64 "type-case default I64" 0L (default_source "default {I64}") ()

let test_eval_type_case_default_bool () =
  check_bool "type-case default Bool" false (default_source "default {Bool}") ()

let test_eval_type_case_default_unit () =
  check_bool "type-case default Unit" true (default_source "default {Unit} == ()") ()

let test_eval_type_case_default_string_panics () =
  match eval_source (default_source "default {String}") with
  | exception Nbe.EvalError "no default" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected panic"

let default_or_source call =
  "let default_or : {T : Type} -> T -> T = fun {T : Type} -> fun fallback -> \
   match T with \
   I64 -> 0 \
   | Bool -> false \
   | Unit -> () \
   | Char -> 'a' \
   | String -> \"\" \
   | _ -> fallback \
   end \
   in " ^ call

let test_eval_type_case_default_or_i64 () =
  check_i64 "type-case default_or I64" 0L (default_or_source "default_or {I64} 99") ()

let test_eval_type_case_default_or_bool () =
  check_bool "type-case default_or Bool" false (default_or_source "default_or {Bool} true") ()

let test_eval_type_case_default_or_string () =
  check_bool "type-case default_or String" true (default_or_source "default_or {String} \"fallback\" == \"\"") ()

let test_eval_type_case_default_or_nominal_fallback () =
  check_i64 "type-case default_or nominal fallback" 2L
    ("type Color = Red | Blue in " ^ default_or_source "match default_or {Color} Blue with Red -> 1 | Blue -> 2 end")
    ()

let type_name_source call =
  "let type_name : Type -> String = fun T -> \
   match T with \
   I64 -> \"i64\" \
   | Bool -> \"bool\" \
   | Char -> \"char\" \
   | Unit -> \"unit\" \
   | String -> \"string\" \
   | _ -> \"other\" \
   end \
   in " ^ call

let test_eval_type_case_type_name_i64 () =
  check_bool "type-case type_name I64" true (type_name_source "type_name I64 == \"i64\"") ()

let test_eval_type_case_type_name_string () =
  check_bool "type-case type_name String" true (type_name_source "type_name String == \"string\"") ()

let test_eval_equality_nominal_rejected () =
  match eval_source "type Color = Red in Red == Red" with
  | exception Elaborate.ElabError (UnknownTrait "Eq") -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected missing Eq impl"

let test_eval_type_case_nominal_full_application () =
  check_i64 "type-case nominal full application" 1L
    "type Option a = Some a | None in \
     match Option I64 with \
       Option(I64) -> 1 \
     | Option(Bool) -> 2 \
     | _ -> 3 \
     end"
    ()

let test_eval_type_case_nominal_param_bind () =
  check_i64 "type-case nominal param bind" 1L
    "type Option a = Some a | None in \
     match Option I64 with \
       Option x -> match x with I64 -> 1 | _ -> 2 end \
     | _ -> 3 \
     end"
    ()

let test_eval_type_case_nominal_complex_param_pattern () =
  check_i64 "type-case nominal complex param pattern" 1L
    "type Option a = Some a | None in \
     match Option (Option I64) with \
       Option (Option I64 | I64) -> 1 \
     | Option _ -> 2 \
     | _ -> 3 \
     end"
    ()

let nominal_classify_source call =
  "type Option a = Some a | None in \
   let classify : Type -> I64 = fun T -> \
   match T with \
   Option I64 -> 1 \
   | Option _ -> 2 \
   | _ -> 0 \
   end \
   in " ^ call

let test_eval_type_case_nominal_classifier_i64 () =
  check_i64 "type-case nominal classifier I64" 1L (nominal_classify_source "classify (Option I64)") ()

let test_eval_type_case_nominal_classifier_bool () =
  check_i64 "type-case nominal classifier Bool" 2L (nominal_classify_source "classify (Option Bool)") ()

let test_eval_type_case_nominal_classifier_fallback () =
  check_i64 "type-case nominal classifier fallback" 0L (nominal_classify_source "classify I64") ()

let struct_type_classify_source call =
  "let classify : Type -> I64 = fun T -> \
   match T with \
   struct x: I64; _ end -> 1 \
   | struct x: Bool; _ end -> 2 \
   | struct y: p; _ end -> match p with String -> 3 | _ -> 4 end \
   | _ -> 0 \
   end \
   in " ^ call

let test_eval_type_case_struct_field_i64 () =
  check_i64 "type-case struct field I64" 1L
    (struct_type_classify_source "type Point = {x: I64; y: Bool} in classify Point")
    ()

let test_eval_type_case_struct_field_bool () =
  check_i64 "type-case struct field Bool" 2L
    (struct_type_classify_source "type Point = {x: Bool} in classify Point")
    ()

let test_eval_type_case_struct_field_binder () =
  check_i64 "type-case struct field binder" 3L
    (struct_type_classify_source "type Point = {y: String; z: I64} in classify Point")
    ()

let test_eval_type_case_struct_field_fallback () =
  check_i64 "type-case struct field fallback" 0L (struct_type_classify_source "classify I64") ()

let test_eval_type_case_struct_closed_rejects_extra () =
  check_i64 "type-case struct closed rejects extra" 2L
    "type Point = {x: I64; y: Bool} in \
     match Point with \
     struct x: I64 end -> 1 \
     | struct x: I64; _ end -> 2 \
     | _ -> 3 \
     end"
    ()

let test_eval_handler_same_match_branch_effect () =
  check_i64 "handler same match branch effect" 43L
    "let Ping = effect hit : I64 -> I64 end in \
     match perform Ping.hit 1 with \
       x -> x \
     | effect Ping.hit n -> \
         if n == 1 then \
           let y = perform Ping.hit 42 in \
           y + 1 \
         else \
           resume n \
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

let () =
  Alcotest.run "core"
    [
      ( "eval",
        [
          Alcotest.test_case "atom" `Quick (check_i64 "atom" 42L "42");
          Alcotest.test_case "lam+ap" `Quick (check_i64 "lam+ap" 7L "(fun x -> x) 7");
          Alcotest.test_case "apply twice" `Quick
            (check_i64 "apply twice" 5L
               "let twice : (I64 -> I64) -> I64 -> I64 = fun f -> fun x -> f (f x) in \
                let inc : I64 -> I64 = fun n -> n + 1 in twice inc 3");
          Alcotest.test_case "let" `Quick (check_i64 "let" 5L "let x : I64 = 5 in x");
          Alcotest.test_case "if true" `Quick (check_i64 "if true" 1L "if true then 1 else 2");
          Alcotest.test_case "if false" `Quick (check_i64 "if false" 2L "if false then 1 else 2");
          Alcotest.test_case "prod" `Quick test_eval_prod;
          Alcotest.test_case "proj" `Quick (check_i64 "proj" 42L "(42, true).0");
          Alcotest.test_case "dot" `Quick test_eval_dot;
          Alcotest.test_case "module signature argument" `Quick test_eval_module_signature_argument;
          Alcotest.test_case "module signature extra field" `Quick test_eval_module_signature_extra_field;
          Alcotest.test_case "signature sugar argument" `Quick test_eval_signature_sugar_argument;
          Alcotest.test_case "module signature functor" `Quick test_eval_module_signature_functor;
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
              match eval_source "panic {I64} \"test message\"" with
              | exception Nbe.EvalError "test message" -> ()
              | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
              | _ -> Alcotest.fail "expected panic");
          Alcotest.test_case "fix" `Quick
            (check_i64 "fix" 0L
               "let rec f : Bool -> I64 = fun x -> if x then 0 else f true in f false");
          Alcotest.test_case "rec sum" `Quick
            (check_i64 "rec sum" 15L
               "let rec sum : I64 -> I64 = fun n -> if n == 0 then 0 else sum (n - 1) + n in sum 5");
          Alcotest.test_case "factorial" `Quick
            (check_i64 "factorial" 120L
               "let rec fact : I64 -> I64 = fun n -> if n == 0 then 1 else n * fact (n - 1) in fact 5");
          Alcotest.test_case "fibonacci" `Quick
            (check_i64 "fibonacci" 8L
               "let rec fib : I64 -> I64 = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 6");
          Alcotest.test_case "rec count" `Quick
            (check_i64 "rec count" 5L
               "let rec f : I64 -> I64 = fun n -> if n == 5 then 5 else f (n + 1) in f 0");
          Alcotest.test_case "rec not" `Quick
            (check_i64 "rec not" 0L
               "let rec f : Bool -> I64 = fun x -> if x then 0 else f (not x) in f false");
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
          Alcotest.test_case "handler record payload pattern" `Quick test_eval_handler_record_payload_pattern;
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
          Alcotest.test_case "continuation reuse error" `Quick test_eval_continuation_reuse_error;
          Alcotest.test_case "match ctor" `Quick
            (check_i64 "match ctor" 1L
               "type Color = Red | Green in match Red with Red -> 1 | Green -> 2 end");
          Alcotest.test_case "match wildcard" `Quick
            (check_i64 "match wildcard" 99L
               "type Color = Red | Green | Blue in \
                match Green with Red -> 1 | _ -> 99 end");
          Alcotest.test_case "match bind" `Quick
            (check_i64 "match bind" 42L
               "type Option a = Some a | None in \
                match Some 42 with Some(x) -> x | None -> 0 end");
          Alcotest.test_case "match constructor or-pattern binding" `Quick
            (check_i64 "match constructor or-pattern binding" 5L
               "type E = A I64 | B I64 in \
                match B 5 with A(x) | B(x) -> x end");
          Alcotest.test_case "match nested" `Quick
            (check_i64 "match nested" 7L
               "type Option a = Some a | None in \
                match Some (Some 7) with \
                  Some(Some(x)) -> x | Some(None) -> 0 | None -> 0 end");
          Alcotest.test_case "recursive parameterized ADT match" `Quick
            (check_i64 "recursive parameterized ADT match" 1L
               "type List a = Cons (a * List a) | Nil in \
                match Cons (1, Nil) with Cons(p) -> p.0 | Nil -> 0 end");
          Alcotest.test_case "recursive list sum" `Quick
            (check_i64 "recursive list sum" 6L
               "type List a = Cons (a * List a) | Nil in \
                let rec sum : List I64 -> I64 = fun xs -> \
                  match xs with Cons(p) -> p.0 + sum p.1 | Nil -> 0 end in \
                sum (Cons (1, Cons (2, Cons (3, Nil))))");
          Alcotest.test_case "qualified constructor pattern" `Quick
            (check_i64 "qualified constructor pattern" 2L
               "let S = module pub type Color = Red | Green end in \
                match S.Green with S.Red -> 1 | S.Green -> 2 end");
          Alcotest.test_case "qualified nested constructor pattern" `Quick
            (check_i64 "qualified nested constructor pattern" 7L
               "let A = module pub let B = module pub type T = X I64 | Y end end in \
                match A.B.X 7 with A.B.X(n) -> n | A.B.Y -> 0 end");
          Alcotest.test_case "qualified constructor alias pattern" `Quick
            (check_i64 "qualified constructor alias pattern" 1L
               "let S = module pub type Color = Red | Green end in \
                let N = S in match S.Red with N.Red -> 1 | N.Green -> 2 end");
          Alcotest.test_case "match int literal hit" `Quick
            (check_i64 "match int literal hit" 10L
               "match 1 with 1 -> 10 | _ -> 20 end");
          Alcotest.test_case "match int literal default" `Quick
            (check_i64 "match int literal default" 20L
               "match 2 with 1 -> 10 | _ -> 20 end");
          Alcotest.test_case "match literal or-pattern" `Quick
            (check_i64 "match literal or-pattern" 42L
               "match 1 with 0 | 1 -> 42 | _ -> 0 end");
          Alcotest.test_case "match bool literal" `Quick
            (check_i64 "match bool literal" 0L
               "match false with true -> 1 | false -> 0 end");
          Alcotest.test_case "match unit literal" `Quick
            (check_i64 "match unit literal" 7L
               "match () with () -> 7 end");
          Alcotest.test_case "match char literal hit" `Quick
            (check_i64 "match char literal hit" 10L
               "match 'a' with 'a' -> 10 | _ -> 20 end");
          Alcotest.test_case "match char literal default" `Quick
            (check_i64 "match char literal default" 20L
               "match 'b' with 'a' -> 10 | _ -> 20 end");
          Alcotest.test_case "match escaped char literal" `Quick
            (check_i64 "match escaped char literal" 1L
               "match '\\n' with '\\n' -> 1 | _ -> 0 end");
          Alcotest.test_case "match literal binder fallback" `Quick
            (check_i64 "match literal binder fallback" 42L
               "match 42 with 0 -> 0 | x -> x end");
          Alcotest.test_case "match first branch wins" `Quick
            (check_i64 "match first branch wins" 0L
               "match 1 with _ -> 0 | 1 -> 1 end");
          Alcotest.test_case "match tagged payload" `Quick
            (check_i64 "match tagged payload" 42L
               "type Wrapper = W I64 in match W 41 with W(x) -> x + 1 end");
          Alcotest.test_case "match tuple bind" `Quick
            (check_i64 "match tuple bind" 1L
               "match (1, true) with (x, b) -> if b then x else 0 end");
          Alcotest.test_case "match tuple wildcard" `Quick
            (check_i64 "match tuple wildcard" 2L
               "match (1, 2) with (_, y) -> y end");
          Alcotest.test_case "match whole tuple binder" `Quick
            (check_i64 "match whole tuple binder" 1L
               "match (1, true) with p -> p.0 end");
          Alcotest.test_case "match nested tuple" `Quick
            (check_i64 "match nested tuple" 3L
               "match ((1, true), 2) with ((x, _), y) -> x + y end");
          Alcotest.test_case "match tuple literals" `Quick
            (check_i64 "match tuple literals" 9L
               "match (false, 1) with (true, x) -> x | (false, _) -> 9 end");
          Alcotest.test_case "record field access" `Quick
            (check_i64 "record field access" 1L
               "let Point = struct x: I64; y: I64; end in (Point {x = 1; y = 2}).x");
          Alcotest.test_case "parameterized record construction" `Quick
            (check_bool "parameterized record construction" true
               "let Pair = fun {A : Type} {B : Type} -> struct fst: A; snd: B; end in (Pair {fst = 1; snd = true}).snd");
          Alcotest.test_case "record type declaration" `Quick
            (check_i64 "record type declaration" 2L
               "type Point = {x: I64; y: I64} in (Point {x = 1; y = 2}).y");
          Alcotest.test_case "record construction field order" `Quick
            (check_i64 "record construction field order" 30L
               "type Point = {x: I64; y: I64} in \
                let p = Point {y = 20; x = 10} in p.x + p.y");
          Alcotest.test_case "parameterized record type declaration" `Quick
            (check_bool "parameterized record type declaration" true
               "type Pair A B = {fst: A; snd: B} in (Pair {fst = 1; snd = true}).snd");
          Alcotest.test_case "polymorphic record multiple instantiations" `Quick
            (check_i64 "polymorphic record multiple instantiations" 13L
               "type Pair A B = {fst: A; snd: B} in \
                let p1 = Pair {fst = 10; snd = 20} in \
                let p2 = Pair {fst = true; snd = 3} in \
                if p2.fst then p1.fst + p2.snd else 0");
          Alcotest.test_case "parameterized record method" `Quick
            (check_bool "parameterized record method" true
               "let Pair = fun {A : Type} {B : Type} -> struct fst: A; snd: B; pub let swap = fun p -> (p.snd, p.fst) end in (Pair.swap (Pair {fst = 1; snd = true})).0");
          Alcotest.test_case "method uses self" `Quick
            (check_i64 "method uses self" 1L
               "let Box = fun {A : Type} -> struct value: A; pub method get -> self.value end in Box.get (Box {value = 1})");
          Alcotest.test_case "parameterized method uses self" `Quick
            (check_bool "parameterized method uses self" true
               "let Pair = fun {A : Type} {B : Type} -> struct fst: A; snd: B; pub method swap -> (self.snd, self.fst) end in (Pair.swap (Pair {fst = 1; snd = true})).0");
          Alcotest.test_case "method extra parameter" `Quick
            (check_i64 "method extra parameter" 3L
               "let Counter = struct value: I64; pub method add x -> self.value + x end in Counter.add (Counter {value = 1}) 2");
          Alcotest.test_case "method uses Self type" `Quick
            (check_i64 "method uses Self type" 2L
               "let Box = fun {A : Type} -> struct value: A; pub method id (other : Self) -> other.value end in Box.id (Box {value = 1}) (Box {value = 2})");
          Alcotest.test_case "method returns Self" `Quick
            (check_i64 "method returns Self" 1L
               "let Box = struct value: I64; pub method copy -> self end in (Box.copy (Box {value = 1})).value");
          Alcotest.test_case "record pattern shorthand" `Quick
            (check_i64 "record pattern shorthand" 3L
               "let Point = struct x: I64; y: I64; end in \
                match Point {x = 1; y = 2} with Point {x; y} -> x + y end");
          Alcotest.test_case "record pattern reordered" `Quick
            (check_i64 "record pattern reordered" 3L
               "let Point = struct x: I64; y: I64; end in \
                match Point {x = 1; y = 2} with Point {y; x} -> x + y end");
          Alcotest.test_case "record pattern renamed field" `Quick
            (check_i64 "record pattern renamed field" 30L
               "type Point = {x: I64; y: I64} in \
                match Point {x = 10; y = 20} with Point {x = wow; y} -> wow + y end");
          Alcotest.test_case "record pattern partial" `Quick
            (check_i64 "record pattern partial" 3L
               "type Point = {x: I64; y: I64} in \
                match Point {x = 3; y = 4} with Point {x; _} -> x end");
          Alcotest.test_case "record pattern literal dispatch" `Quick
            (check_i64 "record pattern literal dispatch" 4L
               "let Flag = struct flag: Bool; value: I64; end in \
                match Flag {flag = false; value = 3} with \
                Flag {flag = true; value} -> value | Flag {flag = false; value} -> value + 1 end");
          Alcotest.test_case "qualified record pattern" `Quick
            (check_i64 "qualified record pattern" 3L
               "let M = module pub let Point = struct x: I64; y: I64; end end in \
                match M.Point {x = 1; y = 2} with M.Point {x; y} -> x + y end");
          Alcotest.test_case "struct private helper" `Quick
            (check_i64 "struct private helper" 11L
               "let M = module let secret = 10; pub let x = secret + 1 end in M.x");
          Alcotest.test_case "module public function" `Quick
            (check_i64 "module public function" 42L
               "let M = module let helper = fun x -> x * 2; pub let double = helper end in M.double 21");
          Alcotest.test_case "struct multiple public members" `Quick
            (check_i64 "struct multiple public members" 3L
               "let M = module pub let a = 1; pub let b = 2 end in M.a + M.b");
          Alcotest.test_case "open struct values" `Quick
            (check_i64 "open struct values" 52L
               "let M = module pub let x = 42; pub let y = 10 end in open M in x + y");
          Alcotest.test_case "open struct constructors" `Quick
            (check_i64 "open struct constructors" 1L
               "let Color = module pub type Color = Red | Green | Blue end in \
                open Color in match Red with Red -> 1 | Green -> 2 | Blue -> 3 end");
        ] );
      ( "imports",
        [
          Alcotest.test_case "basic import" `Quick
            (check_import_i64 "basic import" [ ("math", "pub let x = 41; pub let y = x + 1") ] 42L
               "let M = import \"math\" in M.y");
          Alcotest.test_case "imported public function" `Quick
            (check_import_i64 "imported public function" [ ("math", "pub let double = fun x -> x + x") ] 10L
               "let M = import \"math\" in M.double 5");
          Alcotest.test_case "nested import" `Quick
            (check_import_i64 "nested import"
               [ ("base", "pub let x = 42"); ("wrapper", "pub let M = import \"base\"") ] 42L
               "let W = import \"wrapper\" in W.M.x");
          Alcotest.test_case "repeated import" `Quick
            (check_import_i64 "repeated import" [ ("m", "pub let x = 21") ] 42L
               "let A = import \"m\" in let B = import \"m\" in A.x + B.x");
          Alcotest.test_case "imported ADT match" `Quick
            (check_import_i64 "imported ADT match"
               [ ("color", "pub type Color = Red | Green | Blue; pub let default = Green") ] 2L
               "let C = import \"color\" in match C.default with C.Red -> 1 | C.Green -> 2 | C.Blue -> 3 end");
          Alcotest.test_case "imported record field access" `Quick
            (check_import_i64 "imported record field access"
               [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 1L
               "let S = import \"shapes\" in (S.Point {x = 1; y = 2}).x");
          Alcotest.test_case "imported record pattern" `Quick
            (check_import_i64 "imported record pattern"
               [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 3L
               "let S = import \"shapes\" in match S.Point {x = 1; y = 2} with S.Point {x; y} -> x + y end");
          Alcotest.test_case "imported record pattern alias" `Quick
            (check_import_i64 "imported record pattern alias"
               [ ("shapes", "pub type Point = {x: I64; y: I64}") ] 3L
               "let S = import \"shapes\" in let Alias = S in match S.Point {x = 1; y = 2} with Alias.Point {x; y} -> x + y end");
          Alcotest.test_case "imported method uses self" `Quick
            (check_import_i64 "imported method uses self"
               [ ("box", "pub let Box = struct value: I64; pub method get -> self.value end") ] 1L
               "let B = import \"box\" in B.Box.get (B.Box {value = 1})");
          Alcotest.test_case "imported method uses Self" `Quick
            (check_import_i64 "imported method uses Self"
               [ ("box", "pub let Box = struct value: I64; pub method copy (other : Self) -> other.value end") ]
               2L
               "let B = import \"box\" in B.Box.copy (B.Box {value = 1}) (B.Box {value = 2})");
          Alcotest.test_case "imported nested constructor pattern" `Quick
            (check_import_i64 "imported nested constructor pattern"
               [ ("nested", "pub let M = module pub type T = X I64 | Y end") ] 7L
               "let N = import \"nested\" in match N.M.X 7 with N.M.X(n) -> n | N.M.Y -> 0 end");
          Alcotest.test_case "imported module alias pattern" `Quick
            (check_import_i64 "imported module alias pattern"
               [ ("color", "pub type Color = Red | Green") ] 1L
               "let C = import \"color\" in let Alias = C in match C.Red with Alias.Red -> 1 | Alias.Green -> 2 end");
          Alcotest.test_case "imported public effect handler" `Quick
            (check_import_i64 "imported public effect handler"
               [ ("effects", "pub let Exc = effect raise : I64 -> I64 end") ] 2L
               "let E = import \"effects\" in match perform E.Exc.raise 1 with x -> x | effect E.Exc.raise n -> n + 1 end");
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
    ]
