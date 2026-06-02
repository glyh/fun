open Core

let () =
  Printexc.register_printer (function
    | Elaborate.ElabError e ->
        let open Elaborate in
        Some (Printf.sprintf "ElabError(%s)" (match e with
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
    | Core_loader.CircularImport path -> Some ("CircularImport \"" ^ path ^ "\"")
    | Core_loader.ImportNotFound path -> Some ("ImportNotFound \"" ^ path ^ "\"")
    | Unify.UnifyError e ->
        let open Unify in
        Some (Printf.sprintf "UnifyError(%s)" (match e with
          | NonLinearSpine -> "NonLinearSpine"
          | NonVariableInSpine -> "NonVariableInSpine"
          | VarNotInSpine l -> Printf.sprintf "VarNotInSpine %d" l
          | NeutralVarNotInSpine l -> Printf.sprintf "NeutralVarNotInSpine %d" l
          | OccursCheck -> "OccursCheck"
          | CannotUnify msg -> "CannotUnify \"" ^ msg ^ "\""
          | TupleLengthMismatch -> "TupleLengthMismatch"
          | SpineLengthMismatch -> "SpineLengthMismatch"
          | NeutralHeadMismatch -> "NeutralHeadMismatch"
          | FrameMismatch -> "FrameMismatch"
          | StructFieldMismatch -> "StructFieldMismatch"
          | NominalMismatch (n1, n2) ->
              Printf.sprintf "NominalMismatch(%s, %s)" n1 n2
          | EffectMismatch (e1, e2) ->
              Printf.sprintf "EffectMismatch(%s, %s)" e1 e2
          | EffectRowMismatch -> "EffectRowMismatch"))
    | _ -> None)

let parse_expr = Parse_expand.parse_expr
let pi explicitness domain codomain = Pi { explicitness; domain; effects = empty_effect_row; codomain }

let elab source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  Elaborate.on_expr ctx expr

let elab_with_loader loader source =
  let expr = parse_expr source in
  let ctx = Elaborate.init_ctx () in
  Elaborate.on_expr ~loader ctx expr

let with_modules modules f =
  let dir = Filename.temp_dir "fun_core_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = Core_loader.create ~base_dir:dir in
  f loader

let check_import_type modules source expected () =
  with_modules modules (fun loader ->
      let _core, ty = elab_with_loader loader source in
      let mc = MetaContext.create () in
      let expected_val = Nbe.eval mc [] expected in
      if not (Nbe.conv mc 0 ty expected_val) then Alcotest.fail "type mismatch")

let import_elab_fail modules source () =
  with_modules modules (fun loader ->
      match elab_with_loader loader source with
      | exception Elaborate.ElabError _ -> ()
      | exception Unify.UnifyError _ -> ()
      | _ -> Alcotest.fail "expected elaboration error")

let check_type source expected () =
  let _core, ty = elab source in
  let mc = MetaContext.create () in
  let expected_val = Nbe.eval mc [] expected in
  if not (Nbe.conv mc 0 ty expected_val) then
    Alcotest.fail
      (Printf.sprintf "type mismatch: got %s"
         (let mc2 = MetaContext.create () in
          let q = Nbe.quote mc2 0 ty in
          match q with
          | AtomTy Atom_ty.TI64 -> "I64"
          | AtomTy Atom_ty.TBool -> "Bool"
          | AtomTy Atom_ty.TUnit -> "Unit"
          | Pi _ -> "<pi>"
          | ProdTy _ -> "<prod>"
          | _ -> "<other>"))

let elab_ok source () =
  let _core, _ty = elab source in
  ()

let elab_fail source () =
  match elab source with
  | exception Elaborate.ElabError _ -> ()
  | exception Unify.UnifyError _ -> ()
  | _ -> Alcotest.fail "expected elaboration error"

let constants =
  [
    Alcotest.test_case "int" `Quick (check_type "42" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "unit" `Quick (check_type "()" (AtomTy Atom_ty.TUnit));
    Alcotest.test_case "true" `Quick (check_type "true" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "false" `Quick (check_type "false" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "char" `Quick (check_type "'a'" (AtomTy Atom_ty.TChar));
    Alcotest.test_case "string" `Quick (check_type "\"hello\"" (AtomTy Atom_ty.TString));
    Alcotest.test_case "string type" `Quick (check_type "String" U);
    Alcotest.test_case "absurd type" `Quick (check_type "Absurd" U);
  ]

let let_bindings =
  [
    Alcotest.test_case "simple let" `Quick
      (check_type "do x = 1; x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "let bool" `Quick
      (check_type "do b = true; b end" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "let shadowing" `Quick
      (check_type "do x = true; x = 1; x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "lambda shadows outer let" `Quick
      (check_type "do x = true; (fn(x) -> x : I64 -> I64)(1) end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "non-rec let rhs sees outer binding" `Quick
      (check_type "do x = 1; x = x; x end" (AtomTy Atom_ty.TI64));
  ]

let conditionals =
  [
    Alcotest.test_case "simple if" `Quick
      (check_type "if true do 1 else 2 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "if branches must match" `Quick
      (elab_fail "if true do 1 else false end");
    Alcotest.test_case "nested if" `Quick
      (check_type "if true do if false do 1 else 2 end else 3 end" (AtomTy Atom_ty.TI64));
  ]

let lambdas =
  [
    Alcotest.test_case "application" `Quick
      (check_type "((fn(x) -> x) : I64 -> I64)(42)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "apply non-function" `Quick
      (elab_fail "1(2)");
    Alcotest.test_case "annotated identity" `Quick
      (check_type "(fn(x) -> x : I64 -> I64)"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
    Alcotest.test_case "bool function" `Quick
      (check_type "(fn(x) -> x : Bool -> Bool)"
         (pi Explicit (AtomTy Atom_ty.TBool) (AtomTy Atom_ty.TBool)));
    Alcotest.test_case "higher-order twice" `Quick
      (check_type
         "do twice : (I64 -> I64) -> I64 -> I64 = fn(f, x) -> f(f(x)); twice end"
         (pi Explicit
            (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64))
            (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64))));
  ]

let annotations =
  [
    Alcotest.test_case "int annotation" `Quick
      (check_type "(42 : I64)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "bool annotation" `Quick
      (check_type "(true : Bool)" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "char annotation" `Quick
      (check_type "('a' : Char)" (AtomTy Atom_ty.TChar));
    Alcotest.test_case "let with annotation" `Quick
      (check_type "do x : I64 = 42; x end" (AtomTy Atom_ty.TI64));
  ]

let tuples =
  [
    Alcotest.test_case "pair" `Quick
      (check_type "(1, true)" (ProdTy [ AtomTy Atom_ty.TI64; AtomTy Atom_ty.TBool ]));
    Alcotest.test_case "triple" `Quick
      (check_type "(1, 2, 3)"
         (ProdTy [ AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64 ]));
  ]

let operators =
  [
    Alcotest.test_case "add" `Quick (check_type "1 + 2" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "compare" `Quick (check_type "1 == 2" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "bool equality" `Quick (check_type "true == false" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "char equality" `Quick (check_type "'a' == 'a'" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "unit equality" `Quick (check_type "() == ()" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "type alias equality" `Quick
      (check_type "do MyInt = I64; (1 : MyInt) == (2 : MyInt) end" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "operator as value" `Quick (check_type "(==)(1)(1)" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "polymorphic helper" `Quick
      (check_type "do same : [A : Type] -> A -> A -> Bool = fn[A : Type] -> (==)[A]; same(1, 1) end" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "complex" `Quick
      (check_type "do x = 1 + 2; x * 3 end" (AtomTy Atom_ty.TI64));
  ]

let equality_rejections =
  [
    Alcotest.test_case "mismatch rejection" `Quick (elab_fail "1 == true");
    Alcotest.test_case "nominal equality requires impl" `Quick
      (elab_fail "do type Color = Red; Red == Red end");
    Alcotest.test_case "record equality requires impl" `Quick
      (elab_fail "do type Point = {x: I64}; Point{x = 1} == Point{x = 1} end");
  ]

let dependent =
  [
    Alcotest.test_case "Type as value" `Quick
      (check_type "(I64 : Type)" U);
    Alcotest.test_case "type-head match I64" `Quick
      (check_type "match I64 do I64 -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Bool" `Quick
      (check_type "match Bool do Bool -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Char" `Quick
      (check_type "match Char do Char -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Unit" `Quick
      (check_type "match Unit do Unit -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open Type match fallback" `Quick
      (check_type "do classify : Type -> I64 = fn(T) -> match T do I64 -> 1 | _ -> 0 end; classify(Bool) end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-case refines dependent argument" `Quick
      (check_type
         "do is_zeroish : [T : Type] -> T -> Bool = fn[T : Type](x) -> \
          match T do \
          I64 -> x == 0 \
          | Bool -> x == false \
          | Unit -> true \
          | Char -> x == 'a' \
          | _ -> false \
          end; is_zeroish end"
         (Pi { explicitness = Implicit; domain = U; effects = empty_effect_row; codomain = Pi { explicitness = Explicit; domain = Var 0; effects = empty_effect_row; codomain = AtomTy Atom_ty.TBool } }));
    Alcotest.test_case "type-case rejects invalid refined branch" `Quick
      (elab_fail
         "do bad : (T : Type) -> T -> Bool = fn(T : Type, x) -> \
          match T do \
          I64 -> x == false \
          | _ -> false \
          end; bad end");
    Alcotest.test_case "type-case default refines return type" `Quick
      (check_type
         "do default : [T : Type] -> T = fn[T : Type] -> \
          match T do \
          I64 -> 0 \
          | Bool -> false \
          | Unit -> () \
          | Char -> 'a' \
           | _ -> panic(\"no default\") \
          end; default end"
         (Pi { explicitness = Implicit; domain = U; effects = empty_effect_row; codomain = Var 0 }));
    Alcotest.test_case "type-case default with fallback" `Quick
      (elab_ok
         "do default_or : [T : Type] -> T -> T = fn[T : Type](fallback) -> \
          match T do \
          I64 -> 0 \
          | Bool -> false \
          | Unit -> () \
          | Char -> 'a' \
          | String -> \"\" \
          | _ -> fallback \
          end; default_or end");
    Alcotest.test_case "type-case string classifier" `Quick
      (elab_ok
         "do type_name : Type -> String = fn(T) -> \
          match T do \
          I64 -> \"i64\" \
          | Bool -> \"bool\" \
          | Char -> \"char\" \
          | Unit -> \"unit\" \
          | String -> \"string\" \
          | _ -> \"other\" \
          end; type_name end");
    Alcotest.test_case "type-case nominal classifier" `Quick
      (elab_ok
         "do type Option a = Some a | None; \
          classify : Type -> I64 = fn(T) -> \
          match T do \
          Option(I64) -> 1 \
          | Option _ -> 2 \
          | _ -> 0 \
          end; classify end");
    Alcotest.test_case "type-case struct field type binder" `Quick
      (elab_ok
         "do classify : Type -> I64 = fn(T) -> \
          match T do \
          struct x: p; _ end -> match p do I64 -> 1 | _ -> 2 end \
          | _ -> 0 \
          end; classify end");
    Alcotest.test_case "type-case struct field type pattern" `Quick
      (elab_ok
         "do classify : Type -> I64 = fn(T) -> \
          match T do \
          struct x: I64; _ end -> 1 \
          | struct x: Bool; _ end -> 2 \
          | _ -> 0 \
          end; classify end");
    Alcotest.test_case "type-case duplicate struct field pattern rejects" `Quick
      (elab_fail
         "match I64 do \
          struct x: I64; x: Bool; _ end -> 1 \
          | _ -> 0 \
          end");
    Alcotest.test_case "type-case struct pattern on non-Type rejects" `Quick
      (elab_fail "match 1 do struct x: I64; _ end -> 1 | _ -> 0 end");
    Alcotest.test_case "type-case unresolved uppercase pattern rejects" `Quick
      (elab_fail
         "do type Option a = Some a | None; \
          match Option(I64) do Option X -> I64 | _ -> Bool end end");
    Alcotest.test_case "type-passing identity" `Quick
      (elab_ok
         "((fn(T : Type, x : T) -> x) : Type -> I64 -> I64)(I64)(42)");
    Alcotest.test_case "type-level if" `Quick
      (elab_ok
         "do choose : Type -> Type -> Bool -> Type = fn(a, b, c) -> if c do a else b end; (42 : choose(I64, Bool, true)) end");
    Alcotest.test_case "dependent return type" `Quick
      (elab_ok
         "do f : Bool -> Type = fn(b) -> if b do I64 else Bool end; (42 : f(true)) end");
    Alcotest.test_case "dependent return type false" `Quick
      (elab_ok
         "do f : Bool -> Type = fn(b) -> if b do I64 else Bool end; (true : f(false)) end");
    Alcotest.test_case "dependent mismatch" `Quick
      (elab_fail
         "do f : Bool -> Type = fn(b) -> if b do I64 else Bool end; (true : f(true)) end");
  ]

let meta_solving =
  [
    Alcotest.test_case "infer identity arg" `Quick
      (check_type "((fn(x) -> x) : I64 -> I64)(42)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "infer through let" `Quick
      (check_type "do f : I64 -> I64 = fn(x) -> x; f(42) end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "infer lambda param from body" `Quick
      (check_type "(fn(x) -> x + 1 : I64 -> I64)"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
  ]

let structs =
  [
    Alcotest.test_case "empty module" `Quick
      (elab_ok "module end");
    Alcotest.test_case "open module" `Quick
      (check_type
         "do S = module pub x = 42 end; open S; x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open struct constructors" `Quick
      (elab_ok
         "do Color = module pub type Color = Red | Green | Blue end; \
          open Color; Red end");
    Alcotest.test_case "struct with pub fields" `Quick
      (elab_ok
         "do S = module pub x = 1; pub y = true end; open S; if y do x else 0 end end");
    Alcotest.test_case "nested struct open" `Quick
      (check_type
         "do Outer = module pub Inner = module pub val = 42 end end; open Outer; open Inner; val end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open only imports pub" `Quick
      (elab_fail
         "do S = module x = 42 end; open S; x end");
    Alcotest.test_case "field access" `Quick
      (check_type
         "do S = module pub x = 42 end; S.x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "field access boolean" `Quick
      (check_type
         "do S = module pub x = 1; pub y = true end; S.y end"
         (AtomTy Atom_ty.TBool));
    Alcotest.test_case "nested field access" `Quick
      (check_type
         "do Outer = module pub Inner = module pub val = 42 end end; Outer.Inner.val end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "field not found" `Quick
      (elab_fail "do S = module pub x = 1 end; S.y end");
    Alcotest.test_case "private not accessible" `Quick
      (elab_fail "do S = module x = 42 end; S.x end");
    Alcotest.test_case "field decls" `Quick
      (elab_ok "struct x: I64; y: Bool; end");
    Alcotest.test_case "field decls with pub binding" `Quick
      (elab_ok "struct x: I64; pub fourty_two = 42 end");
    Alcotest.test_case "pass record through function" `Quick
      (elab_ok
         "do Point = struct x: I64; end; (fn(p) -> p.x)(Point{x = 42}) end");
    Alcotest.test_case "module signature argument" `Quick
      (check_type
         "(fn(m : module x = I64 end) -> m.x)(module pub x = 1 end)"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "module signature allows extra fields" `Quick
      (check_type
         "(fn(m : module x = I64 end) -> m.x)(module pub x = 1; pub y = true end)"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "signature sugar argument" `Quick
      (check_type
         "(fn(m : sig x : I64 end) -> m.x)(module pub x = 1 end)"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "module signature missing field rejected" `Quick
      (elab_fail
         "(fn(m : module x = I64 end) -> m.x)(module pub y = 1 end)");
    Alcotest.test_case "module signature wrong field type rejected" `Quick
      (elab_fail
         "(fn(m : module x = I64 end) -> m.x)(module pub x = true end)");
    Alcotest.test_case "module signature private field rejected" `Quick
      (elab_fail
         "(fn(m : module x = I64 end) -> m.x)(module x = 1 end)");
    Alcotest.test_case "record struct does not satisfy module signature" `Quick
      (elab_fail
         "do Point = struct x: I64; end; (fn(m : module x = I64 end) -> m.x)(Point) end");
    Alcotest.test_case "private used by pub" `Quick
      (check_type
         "do S = module helper = 42; pub x = helper end; S.x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record construction" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; Point{x = 1; y = 2} end"
         (Struct { con_fields = [ ("x", AtomTy Atom_ty.TI64); ("y", AtomTy Atom_ty.TI64) ]; bindings = []; partial = false }));
    Alcotest.test_case "record field access" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; (Point{x = 1; y = 2}).x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized record construction" `Quick
      (elab_ok
         "do Pair = fn[A : Type, B : Type] -> struct fst: A; snd: B; end; (Pair[I64, Bool]{fst = 1; snd = true}).snd end");
    Alcotest.test_case "record type declaration" `Quick
      (check_type
         "do type Point = {x: I64; y: I64}; (Point{x = 1; y = 2}).x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized record type declaration" `Quick
      (check_type
         "do type Pair A B = {fst: A; snd: B}; (Pair{fst = 1; snd = true}).snd end"
         (AtomTy Atom_ty.TBool));
    Alcotest.test_case "record type declaration pattern" `Quick
      (check_type
         "do type Point = {x: I64; y: I64}; match Point{x = 1; y = 2} do Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record construction field order" `Quick
      (check_type
         "do type Point = {x: I64; y: I64}; p = Point{y = 20; x = 10}; p.x + p.y end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "polymorphic record multiple instantiations" `Quick
      (check_type
         "do type Pair A B = {fst: A; snd: B}; \
          p1 = Pair{fst = 10; snd = 20}; \
          p2 = Pair{fst = true; snd = 3}; \
          if p2.fst do p1.fst + p2.snd else 0 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record type declaration missing field" `Quick
      (elab_fail "do type Point = {x: I64; y: I64}; Point{x = 1} end");
    Alcotest.test_case "record type declaration unknown field" `Quick
      (elab_fail "do type Point = {x: I64}; Point{x = 1; y = 2} end");
    Alcotest.test_case "record construction duplicate field" `Quick
      (elab_fail "do type Point = {x: I64}; Point{x = 1; x = 2} end");
    Alcotest.test_case "record declaration duplicate field" `Quick
      (elab_fail "do type Point = {x: I64; x: Bool}; Point end");
    Alcotest.test_case "record type declaration same recursion" `Quick
      (elab_ok "do type Option A = Some A | None; type List A = {meta: A; next: Option(List(A))}; List end");
    Alcotest.test_case "recursive record construction" `Quick
      (elab_ok "do type Option A = Some A | None; type List A = {meta: A; next: Option(List(A))}; List{meta = 1; next = None} end");
    Alcotest.test_case "recursive record rejects non-self payload" `Quick
      (elab_fail "do type Option A = Some A | None; type List A = {meta: A; next: Option(List(A))}; List{meta = 1; next = Some(2)} end");
    Alcotest.test_case "record rewrite respects type name shadowing" `Quick
      (elab_ok "do type R A = {x: (fn(R) -> R)(I64)}; R end");
    Alcotest.test_case "record rewrite respects parameter shadowing" `Quick
      (elab_ok "do type R A = {x: (fn(A) -> A)(I64)}; R end");
    Alcotest.test_case "record type declaration changed recursion rejected" `Quick
      (elab_fail "do type Bad(A, B) = {x: Bad(B, A)}; Bad end");
    Alcotest.test_case "method uses self" `Quick
      (check_type
         "do Box = fn[A : Type] -> struct value: A; pub method get() -> self.value end; Box[I64].get(Box[I64]{value = 1}) end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized method uses self" `Quick
      (check_type
         "do Pair = fn[A : Type, B : Type] -> struct fst: A; snd: B; pub method swap() -> (self.snd, self.fst) end; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = true})).0 end"
         (AtomTy Atom_ty.TBool));
    Alcotest.test_case "method extra parameter" `Quick
      (check_type
         "do Counter = struct value: I64; pub method add(x) do self.value + x end end; Counter.add(Counter{value = 1})(2) end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "method uses Self type" `Quick
      (check_type
         "do Box = fn[A : Type] -> struct value: A; pub method id(other : Self) -> other.value end; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "Self in let binding" `Quick
      (elab_ok
         "do Box = struct value: I64; pub id = fn(b : Self) -> b.value end; Box.id(Box{value = 1}) end");
    Alcotest.test_case "self outside method" `Quick
      (elab_fail "self");
    Alcotest.test_case "self in let binding" `Quick
      (elab_fail "do Box = struct value: I64; pub bad = self.value end; Box.bad end");
    Alcotest.test_case "record construction missing field" `Quick
      (elab_fail "do Point = struct x: I64; y: I64; end; Point{x = 1} end");
    Alcotest.test_case "record construction unknown field" `Quick
      (elab_fail "do Point = struct x: I64; end; Point{x = 1; y = 2} end");
    Alcotest.test_case "record construction duplicate field" `Quick
      (elab_fail "do Point = struct x: I64; end; Point{x = 1; x = 2} end");
  ]

let functors =
  [
    Alcotest.test_case "identity functor" `Quick
      (check_type
         "do Double = fn(M : module x = I64 end) -> module pub doubled = M.x + M.x end; (Double(module pub x = 21 end)).doubled end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "functor pass through" `Quick
      (elab_ok
         "do F = fn(M : module x = I64 end) -> module pub y = M.x end; A = module pub x = 1 end; B = F(A); B.y end");
    Alcotest.test_case "functor with private helper" `Quick
      (check_type
         "do F = fn(M : module x = I64 end) -> module tmp = M.x; pub y = tmp + 1 end; (F(module pub x = 1 end)).y end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "compose functors" `Quick
      (elab_ok
         "do F = fn(M : module x = I64 end) -> module pub a = M.x end; G = fn(N : module a = I64 end) -> module pub b = N.a end; (G(F(module pub x = 1 end))).b end");
    Alcotest.test_case "higher order functor" `Quick
      (elab_ok
         "do Apply = fn(F, M : module x = I64 end) -> F(M); Apply(fn(M : module x = I64 end) -> module pub z = M.x end)(module pub x = 1 end) end");
  ]

let tuple_proj =
  [
    Alcotest.test_case "proj first" `Quick
      (check_type "(1, true).0" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj second" `Quick
      (check_type "(1, true).1" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "proj triple" `Quick
      (check_type "(1, 2, 3).2" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj chain" `Quick
      (check_type "((1, true), 42).0.1" (AtomTy Atom_ty.TBool));
    Alcotest.test_case "proj from let" `Quick
      (check_type "do p = (1, true); p.0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj type error" `Quick
      (elab_fail "42.0");
  ]

let adts =
  [
    Alcotest.test_case "constructor type" `Quick (fun () ->
      let _core, ty = elab "do type Color = Red | Green | Blue; Red end" in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "constructor should have type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "multiple constructors" `Quick (fun () ->
      (* All constructors of the same ADT have the same nominal type *)
      let _core, ty =
        elab "do type Color = Red | Green | Blue; \
              _ : Color = Red; \
              _ : Color = Green; Color end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU");
    Alcotest.test_case "shadowing" `Quick (fun () ->
      let _core, ty =
        elab
          "do type A = X | Y; \
           type B = X | Z; X end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B (most recent)"
      | _ -> Alcotest.fail "expected nominal");
    Alcotest.test_case "undefined constructor" `Quick
      (elab_fail "do type Color = Red | Green; Blue end");
    Alcotest.test_case "type Color accessible in body" `Quick (fun () ->
      let _core, ty = elab "do type Color = Red | Green | Blue; Color end" in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU (Type)");
    Alcotest.test_case "pub type inside struct" `Quick (fun () ->
      let _core, ty =
        elab
          "do S = module \
           pub type Color = Red | Green | Blue \
           end; S.Color end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "S.Color should have type VU (Type)");
    Alcotest.test_case "pub type ctor via dot" `Quick (fun () ->
      let _core, ty =
        elab
          "do S = module \
           pub type Color = Red | Green | Blue \
           end; S.Red end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "S.Red should have nominal type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "private type not visible via dot" `Quick
      (elab_fail
         "do S = module \
          type Color = Red | Green | Blue \
          end; S.Red end");
    Alcotest.test_case "private type visible to later binding" `Quick (fun () ->
      let _core, ty =
        elab
          "do S = module \
           type Color = Red | Green | Blue; \
           pub default = Red \
           end; S.default end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "private type should be usable inside struct"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two structs, distinct nominal ids" `Quick (fun () ->
      let _core, _ty =
        elab
          "do S = module pub type Color = Red | Green end; \
           T = module pub type Color = Blue end; \
           _ : S.Color = S.Red; () end"
      in
      ());
    Alcotest.test_case "distinct nominal ids don't unify" `Quick
      (elab_fail
         "do S = module pub type Color = Red | Green end; \
          T = module pub type Color = Blue end; \
          _ : T.Color = S.Red; () end");
    Alcotest.test_case "parameterized ADT with payload" `Quick (fun () ->
      let _core, ty =
        elab "do type Option a = Some a | None; Some[I64](42) end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "nullary pctor has nominal type" `Quick (fun () ->
      let _core, ty =
        elab "do type Option a = Some a | None; None[I64] end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "None I64 should have type Option(I64)"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor partial application" `Quick (fun () ->
      let _core, ty =
        elab "do type Option a = Some a | None; Some[I64] end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VPi _ -> ()  (* Some[I64] : I64 -> Option(I64) *)
      | _ -> Alcotest.fail "Some[I64] should be a function type");
    Alcotest.test_case "multiple type params" `Quick (fun () ->
      let _core, ty =
        elab "do type Result a e = Ok a | Err e; Ok[I64, Bool](42) end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Result") then
            Alcotest.fail "Ok[I64, Bool](42) should have type Result I64 Bool"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor via let binding" `Quick (fun () ->
      let _core, ty =
        elab "do type Option a = Some a | None; \
              f = Some; f[I64](42) end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal via let-bound ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor passed through let" `Quick (fun () ->
      let _core, ty =
        elab "do type Option a = Some a | None; \
              f = Some[I64]; f(42) end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option via let-bound partial ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two pctor adts, distinct types" `Quick (fun () ->
      let _core, ty =
        elab "do type A a = X a | Y; \
              type B a = X a | Z; X[I64](42) end"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "distinct param instantiations don't unify" `Quick
      (elab_fail
         "do type Option a = Some a | None; \
          x : Option(I64) = Some[I64](42); \
          _ : Option(Bool) = x; () end");
    Alcotest.test_case "constructor polymorphism" `Quick
      (elab_ok
         "do type Option a = Some a | None; \
          x = Some(1); \
          y = Some(true); y end");
    Alcotest.test_case "if with ADT branches" `Quick
      (elab_ok
         "do type Result a e = Ok a | Err e; \
          if true do Ok(1) else Err() end end");
    Alcotest.test_case "recursive parameterized ADT" `Quick
      (elab_ok
         "do type List a = Cons(a, List(a)) | Nil; \
         Cons(1, Nil) end");
    Alcotest.test_case "recursive parameterized ADT match" `Quick
      (check_type
         "do type List a = Cons(a, List(a)) | Nil; \
         match Cons(1, Nil) do Cons(x, xs) -> x | Nil -> 0 end end"
         (AtomTy Atom_ty.TI64));
  ]

let match_tests =
  [
    Alcotest.test_case "simple match nullary" `Quick
      (check_type
        "do type Color = Red | Green | Blue; \
         (match Red do Red -> 1 | Green -> 2 | Blue -> 3 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match with payload" `Quick
      (check_type
        "do type Option a = Some a | None; \
         (match Some[I64](42) do Some(x) -> x | None -> 0 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match with wildcard" `Quick
      (check_type
        "do type Color = Red | Green | Blue; \
         (match Red do Red -> 1 | _ -> 0 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match bind variable" `Quick
      (check_type
        "do type Color = Red | Green; \
         (match Red do x -> 1 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "pattern binder shadows outer name" `Quick
      (check_type "do x = true; match 1 do x -> x end end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match non ADT wildcard" `Quick
      (check_type "match 42 do _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match int literal" `Quick
      (check_type "match 42 do 42 -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match bool literals" `Quick
      (check_type "match true do true -> 1 | false -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match unit literal" `Quick
      (check_type "match () do () -> 1 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match char literal" `Quick
      (check_type "match 'a' do 'a' -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match escaped char literal" `Quick
      (check_type "match '\\n' do '\\n' -> 1 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "literal type mismatch" `Quick
      (elab_fail "match 1 do true -> 0 | _ -> 1 end");
    Alcotest.test_case "char literal type mismatch" `Quick
      (elab_fail "match 'a' do 1 -> 0 | _ -> 1 end");
    Alcotest.test_case "non-exhaustive bool literal" `Quick
      (elab_fail "match true do true -> 1 end");
    Alcotest.test_case "non-exhaustive int literal" `Quick
      (elab_fail "match 1 do 1 -> 1 end");
    Alcotest.test_case "non-exhaustive char literal" `Quick
      (elab_fail "match 'a' do 'a' -> 1 end");
    Alcotest.test_case "match literal or-pattern" `Quick
      (check_type "match 1 do 0 | 1 -> 42 | _ -> 0 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "or-pattern covers bool" `Quick
      (check_type "match true do true | false -> 1 end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "constructor or-pattern" `Quick
      (check_type
         "do type Color = Red | Green | Blue; \
          match Red do Red | Green -> 1 | Blue -> 2 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "constructor or-pattern binding" `Quick
      (check_type
         "do type E = A I64 | B I64; \
          match A(1) do A(x) | B(x) -> x end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "or-pattern binding name mismatch" `Quick
      (elab_fail
         "do type E = A I64 | B I64; \
          match A(1) do A(x) | B(y) -> x end end");
    Alcotest.test_case "or-pattern missing binding" `Quick
      (elab_fail
         "do type E = A I64 | C; \
          match A(1) do A(x) | C -> x end end");
    Alcotest.test_case "non-exhaustive constructor or-pattern" `Quick
      (elab_fail
         "do type Color = Red | Green | Blue; \
          match Red do Red | Green -> 1 end end");
    Alcotest.test_case "match tuple pattern" `Quick
      (check_type "match (1, true) do (x, b) -> if b do x else 0 end end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "tuple or-pattern" `Quick
      (check_type
         "match (true, 1) do (true, x) | (false, x) -> x end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match nested tuple pattern" `Quick
      (check_type "match ((1, true), 2) do ((x, _), y) -> x + y end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "tuple pattern arity mismatch" `Quick
      (elab_fail "match (1, true) do (x, y, z) -> x end");
    Alcotest.test_case "tuple literal type mismatch" `Quick
      (elab_fail "match (1, true) do (true, x) -> x | _ -> 0 end");
    Alcotest.test_case "non-exhaustive tuple literal" `Quick
      (elab_fail "match (true, 1) do (true, x) -> x end");
    Alcotest.test_case "match infers tuple scrutinee" `Quick
      (elab_ok "fn(x) -> match x do (true, y) -> y | (false, y) -> y end");
    Alcotest.test_case "match unknown constructor" `Quick
      (elab_fail "do type Color = Red; match Red do Blue -> 0 end end");
    Alcotest.test_case "match payload arity mismatch" `Quick
      (elab_fail "do type Option a = Some a | None; \
                  match Some[I64](42) do Some -> 0 | None -> 0 end end");
    Alcotest.test_case "match wrong scrutinee type" `Quick
      (elab_fail "do type Color = Red | Green; match 42 do Red -> 1 end end");
    Alcotest.test_case "non-exhaustive missing ctor" `Quick
      (elab_fail "do type Color = Red | Green | Blue; \
                  match Red do Red -> 1 | Green -> 2 end end");
    Alcotest.test_case "non-exhaustive nested ADT" `Quick
      (elab_fail
         "do type C = X I64 | Y I64; \
          type B = P C | Q C; \
          type A = M B | N B; \
          match M(P(X(1))) do M(P(X(x))) -> x end end");
    Alcotest.test_case "non-exhaustive partial nested branches" `Quick
      (elab_fail
         "do type Inner = X I64 | Y I64; \
          type Outer = A Inner | B Inner; \
          match A(X(1)) do A(X(x)) -> x | B(X(x)) -> x end end");
    Alcotest.test_case "non-exhaustive single ctor" `Quick
      (elab_fail "do type Option a = Some a | None; \
                  match Some(42) do Some(x) -> x end end");
    Alcotest.test_case "exhaustive with wildcard" `Quick
      (check_type
        "do type Color = Red | Green | Blue; \
         (match Red do Red -> 1 | _ -> 0 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "exhaustive all ctors" `Quick
      (check_type
        "do type Color = Red | Green | Blue; \
         (match Red do Red -> 1 | Green -> 2 | Blue -> 3 end : I64) end"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern" `Quick
      (check_type
         "do S = module pub type Color = Red | Green end; \
          match S.Red do S.Red -> 1 | S.Green -> 2 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified nested constructor pattern" `Quick
      (check_type
         "do A = module pub B = module pub type T = X I64 | Y end end; \
          match A.B.X(7) do A.B.X(n) -> n | A.B.Y -> 0 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern alias" `Quick
      (check_type
         "do S = module pub type Color = Red | Green end; \
          N = S; match S.Red do N.Red -> 1 | N.Green -> 2 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern private" `Quick
      (elab_fail
         "do S = module type Color = Red | Green end; \
          match S.Red do S.Red -> 1 | _ -> 0 end end");
    Alcotest.test_case "qualified constructor pattern wrong nominal" `Quick
      (elab_fail
         "do S = module pub type Color = Red end; \
          T = module pub type Color = Red end; \
          match S.Red do T.Red -> 1 | _ -> 0 end end");
    Alcotest.test_case "record pattern shorthand" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; \
          match Point{x = 1; y = 2} do Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern reordered" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; \
          match Point{x = 1; y = 2} do Point {y; x} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern renamed" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; \
          match Point{x = 1; y = 2} do Point {x = n; y} -> n + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern partial" `Quick
      (check_type
         "do Point = struct x: I64; y: I64; end; \
          match Point{x = 1; y = 2} do Point {x; _} -> x end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern literal dispatch" `Quick
      (check_type
         "do Flag = struct flag: Bool; value: I64; end; \
          match Flag{flag = false; value = 3} do \
          Flag {flag = true; value} -> value | Flag {flag = false; value} -> value + 1 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified record pattern" `Quick
      (check_type
         "do M = module pub Point = struct x: I64; y: I64; end end; \
          match M.Point{x = 1; y = 2} do M.Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified record pattern alias" `Quick
      (check_type
         "do M = module pub Point = struct x: I64; y: I64; end end; \
          N = M; match M.Point{x = 1; y = 2} do N.Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern incomplete" `Quick
      (elab_fail
         "do Point = struct x: I64; y: I64; end; \
          match Point{x = 1; y = 2} do Point {x} -> x end end");
    Alcotest.test_case "record pattern unknown field" `Quick
      (elab_fail
         "do Point = struct x: I64; end; \
          match Point{x = 1} do Point {y; _} -> y end end");
    Alcotest.test_case "record pattern duplicate field" `Quick
      (elab_fail
         "do Point = struct x: I64; end; \
          match Point{x = 1} do Point {x; x} -> x end end");
  ]

let implicit_args =
  [
    Alcotest.test_case "implicit inference Some 42" `Quick (fun () ->
      ignore (elab "do type Option a = Some a | None; \
                    (Some(42) : Option(I64)) end"));
    Alcotest.test_case "explicit implicit Some[I64] 42" `Quick (fun () ->
      ignore (elab "do type Option a = Some a | None; \
                    (Some[I64](42) : Option(I64)) end"));
    Alcotest.test_case "nilary implicit None" `Quick (fun () ->
      ignore (elab "do type Option a = Some a | None; \
                    (None : Option(I64)) end"));
    Alcotest.test_case "partial app implicit" `Quick (fun () ->
      ignore (elab "do type Option a = Some a | None; \
                    (Some[I64] : I64 -> Option(I64)) end"));
    Alcotest.test_case "polymorphic identity" `Quick (fun () ->
      ignore (elab "do id = fn(x) -> x; \
                    _ : I64 = id(42); \
                    _ : Bool = id(true); () end"));
    Alcotest.test_case "match infers scrutinee from patterns" `Quick (fun () ->
      ignore (elab
        "do type Option a = Some a | None; \
         fn(x) -> match x do Some(y) -> y | None -> 0 end end"));
    Alcotest.test_case "match with identity ctor in branch" `Quick (fun () ->
      ignore (elab
        "do type Option a = Some a | None; \
         fn(x) -> match x do Some(y) -> Some(y) | None -> None end end"));
    Alcotest.test_case "match with nested ctor, should be polymorphic" `Quick
      (fun () ->
        ignore (elab
          "do type Option a = Some a | None; \
           fn(x) -> match x do Some(y) -> Some(Some(y)) | None -> None end end"));
    Alcotest.test_case "match same nominal at different instantiations" `Quick
      (fun () ->
        ignore (elab
          "do type Option a = Some a | None; \
           f = fn(x) -> match x do Some(y) -> y | None -> 0 end; \
           g = fn(x) -> match x do Some(y) -> y | None -> true end; \
           _ : I64 = f(Some(1)); \
           _ : Bool = g(Some(true)); () end"));
  ]

let traits =
  [
    Alcotest.test_case "trait declaration" `Quick
      (elab_ok "do trait Eq(A) = sig eq : A -> A -> Bool end; Eq end");
    Alcotest.test_case "impl declaration" `Quick
      (elab_ok "do trait Eq(A) = sig eq : A -> A -> Bool end; impl Eq(I64) = module eq = fn(x, y) -> x == y end; 0 end");
    Alcotest.test_case "trait method dispatch" `Quick
      (check_type
         "do trait Eq(A) = sig eq : A -> A -> Bool end; \
          impl Eq(I64) = module eq = fn(x, y) -> x == y end; Eq.eq(1, 1) end"
         (AtomTy Atom_ty.TBool));
    Alcotest.test_case "trait bound dispatch" `Quick
      (check_type
         "do trait Eq(A) = sig eq : A -> A -> Bool end; \
          impl Eq(I64) = module eq = fn(x, y) -> x == y end; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) -> Eq.eq(x, y); same(1, 1) end"
         (AtomTy Atom_ty.TBool));
    Alcotest.test_case "missing impl rejected" `Quick
      (elab_fail
         "do trait Eq(A) = sig eq : A -> A -> Bool end; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) -> Eq.eq(x, y); same(true, false) end");
    Alcotest.test_case "duplicate trait field rejected" `Quick
      (elab_fail "do trait Bad(A) = sig f : A -> A; f : A -> A end; Bad end");
    Alcotest.test_case "impl missing field rejected" `Quick
      (elab_fail "do trait Eq(A) = sig eq : A -> A -> Bool end; impl Eq(I64) = module end; 0 end");
    Alcotest.test_case "struct impl for Self" `Quick
      (check_type
         "do Point = struct \
             x: I64; \
             pub impl Eq(Self) = module eq = fn(lhs, rhs) -> lhs.x == rhs.x end \
           end; \
           Point{x = 1} == Point{x = 1} end"
         (AtomTy Atom_ty.TBool));
  ]

let effects =
  [
    Alcotest.test_case "effect instance has type Type" `Quick
      (check_type
         "do effect State(S) = sig get : Unit -> S; put : S -> Unit end; State(I64) end"
         U);
    Alcotest.test_case "effect family has function type" `Quick
      (check_type
         "do effect State(S) = sig get : Unit -> S end; State end"
         (pi Explicit U U));
    Alcotest.test_case "same effect family and params convert" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; \
          ((fn(x) -> x : Unit -> Unit can State(I64)) : Unit -> Unit can State(I64)) end");
    Alcotest.test_case "different effect params elaborate" `Quick
      (check_type
         "do effect State(S) = sig get : Unit -> S end; Unit -> I64 can State(Bool) end"
         U);
    Alcotest.test_case "identical signatures remain nominal" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; \
          effect Env(S) = sig get : Unit -> S end; \
          ((fn(x) -> x : Unit -> Unit can State(I64)) : Unit -> Unit can Env(I64)) end");
    Alcotest.test_case "public effect field through dot" `Quick
      (check_type
         "do M = module pub effect State(S) = sig get : Unit -> S end end; M.State(I64) end"
         U);
    Alcotest.test_case "public effect field through open" `Quick
      (check_type
         "do M = module pub effect State(S) = sig get : Unit -> S end end; open M; State(I64) end"
         U);
    Alcotest.test_case "private effect field hidden" `Quick
      (elab_fail
         "do M = module effect State(S) = sig get : Unit -> S end end; M.State(I64) end");
    Alcotest.test_case "private effect usable by public member" `Quick
      (check_type
         "do M = module effect State(S) = sig get : Unit -> S end; pub T = State(I64) end; M.T end"
         U);
    Alcotest.test_case "duplicate operation rejected" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S; get : Unit -> S end; State(I64) end");
    Alcotest.test_case "operation name is not bound" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; get end");
    Alcotest.test_case "imported public effect" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig get : Unit -> S end") ]
         "do E = import \"effects\"; E.State(I64) end" U);
    Alcotest.test_case "effectful arrow has type Type" `Quick
      (check_type
         "do effect IO = sig read : Unit -> I64 end; I64 -> I64 can IO end"
         U);
    Alcotest.test_case "parameterized row has type Type" `Quick
      (check_type
         "do effect State(S) = sig get : Unit -> S end; Unit -> I64 can State(I64) end"
         U);
    Alcotest.test_case "braced multi-effect row has type Type" `Quick
      (check_type
         "do effect State(S) = sig get : Unit -> S end; effect IO = sig read : Unit -> I64 end; Unit -> I64 can {State(I64), IO} end"
         U);
    Alcotest.test_case "EffectRow has type Type" `Quick
      (check_type "EffectRow" U);
    Alcotest.test_case "open effect row has type Type" `Quick
      (check_type
         "do effect IO = sig read : Unit -> I64 end; [r : EffectRow] -> (Unit -> I64 can {IO | r}) end"
         U);
    Alcotest.test_case "tail-only effect row has type Type" `Quick
      (check_type
         "[r : EffectRow] -> (Unit -> I64 can {| r})"
         U);
    Alcotest.test_case "lambda checks against effectful function type" `Quick
      (elab_ok
         "do effect IO = sig read : Unit -> I64 end; (fn(x) -> x : I64 -> I64 can IO) end");
    Alcotest.test_case "row order ignored" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; effect IO = sig read : Unit -> I64 end; ((fn(x) -> x : I64 -> I64 can {IO, State(I64)}) : I64 -> I64 can {State(I64), IO}) end");
    Alcotest.test_case "non-effect row entry rejected" `Quick
      (elab_fail
         "I64 -> I64 can I64");
    Alcotest.test_case "duplicate row entry rejected" `Quick
      (elab_fail
         "do effect IO = sig read : Unit -> I64 end; I64 -> I64 can {IO, IO} end");
    Alcotest.test_case "perform get checks in effectful lambda" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; (fn(_) -> perform State.get () : Unit -> I64 can State(I64)) end");
    Alcotest.test_case "perform put checks in effectful lambda" `Quick
      (elab_ok
         "do effect State(S) = sig put : S -> Unit end; (fn(_) -> perform State.put(42) : Unit -> Unit can State(I64)) end");
    Alcotest.test_case "unknown operation rejected" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; (fn(_) -> perform State.missing () : Unit -> I64 can State(I64)) end");
    Alcotest.test_case "wrong operation argument rejected" `Quick
      (elab_fail
         "do effect State(S) = sig put : S -> Unit end; (fn(_) -> perform State.put(true) : Unit -> Unit can State(I64)) end");
    Alcotest.test_case "pure lambda rejects perform" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; (fn(_) -> perform State.get () : Unit -> I64 can {}) end");
    Alcotest.test_case "pure lambda still checks against effectful type" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; (fn(_) -> 1 : Unit -> I64 can State(I64)) end");
    Alcotest.test_case "effectful call must be accounted for" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; f : Unit -> I64 can State(I64) = fn(_) -> perform State.get (); (fn(_) -> f() : Unit -> I64 can {}) end");
    Alcotest.test_case "effectful call propagates latent row" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; f : Unit -> I64 can State(I64) = fn(_) -> perform State.get (); (fn(_) -> f() : Unit -> I64 can State(I64)) end");
    Alcotest.test_case "unannotated higher-order wrapper threads effects" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; \
          f : Unit -> I64 can State(I64) = fn(_) -> perform State.get (); \
          wrap : (Unit -> I64) -> Unit -> I64 = fn(g) -> fn(_) -> g(); \
          (fn(_) -> wrap(f)() : Unit -> I64 can State(I64)) end");
    Alcotest.test_case "open row accepts concrete prefix effect" `Quick
      (elab_ok
         "do effect IO = sig read : Unit -> I64 end; \
          ((fn[r : EffectRow] -> fn(_) -> perform IO.read ()) : [r : EffectRow] -> (Unit -> I64 can {IO | r})) end");
    Alcotest.test_case "tail-only row accepts empty tail" `Quick
      (elab_ok
         "((fn[r : EffectRow] -> fn(_) -> 1) : [r : EffectRow] -> (Unit -> I64 can {| r}))");
    Alcotest.test_case "extra effect rejects against rigid explicit row" `Quick
      (elab_fail
         "do effect IO = sig read : Unit -> I64 end; \
           effect State(S) = sig get : Unit -> S end; \
           ((fn[r : EffectRow] -> fn(_) do _ = perform IO.read (); perform State.get () end) : [r : EffectRow] -> (Unit -> I64 can {IO | r})) end");
    Alcotest.test_case "inferred perform lambda exposes latent effect" `Quick
      (fun () ->
        let expr = Parse_expand.parse_expr "do effect State(S) = sig get : Unit -> S end; fn(_) -> perform State.get () end" in
        let ctx = Elaborate.init_ctx () in
        let _core, ty, _effects = Elaborate.on_expr_effects ctx expr in
        match Nbe.force ctx.Elaborate.Ctx.metas ty with
        | VPi { effects; _ } when not (List.is_empty effects.effects) -> ()
        | _ -> Alcotest.fail "expected latent State effect");
    Alcotest.test_case "handler removes single-operation effect" `Quick
      (elab_ok
         "do effect Exc = sig raise : I64 -> I64 end; (fn(_) -> match perform Exc.raise(1) do x -> x | effect Exc.raise n -> n + 1 end : Unit -> I64) end");
    Alcotest.test_case "handler continuation type checks" `Quick
      (elab_ok
         "do effect Exc = sig raise : I64 -> I64 end; (fn(_) -> match perform Exc.raise(1) do x -> x | effect Exc.raise n -> resume(n + 1) end : Unit -> I64) end");
    Alcotest.test_case "resume argument type checked" `Quick
      (elab_fail
         "do effect Exc = sig raise : I64 -> I64 end; match perform Exc.raise(1) do x -> x | effect Exc.raise n -> resume(true) end end");
    Alcotest.test_case "lexical resume in nested lambda type checks" `Quick
      (elab_ok
         "do effect Exc = sig raise : I64 -> I64 end; \
          (match perform Exc.raise(1) do x -> x | effect Exc.raise n -> (fn(x) -> resume(x + 1))(n) end : I64) end");
    Alcotest.test_case "handler branch body type checked" `Quick
      (elab_fail
         "do effect Exc = sig raise : I64 -> I64 end; (match perform Exc.raise(1) do x -> x | effect Exc.raise n -> true end : I64) end");
    Alcotest.test_case "tuple effect branch payload checked" `Quick
      (elab_fail
         "do effect Console = sig log : I64 * I64 -> I64 end; \
          match perform Console.log((1, 2)) do x -> x | effect Console.log(only) -> only end end");
    Alcotest.test_case "record effect branch payload checked" `Quick
      (elab_fail
         "do Request = struct value: I64; extra: I64; end; \
          effect Ask = sig prompt : Request -> I64 end; \
          match perform Ask.prompt(Request{value = 1; extra = 2}) do x -> x | effect Ask.prompt Request{missing} -> missing end end");
    Alcotest.test_case "multi-operation handler remains effectful when partial" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S; put : S -> Unit end; (fn(_) -> match perform State.get () do x -> x | effect State.get () -> 0 end : Unit -> I64) end");
    Alcotest.test_case "duplicate effect branch rejected" `Quick
      (elab_fail
         "do effect Exc = sig raise : I64 -> I64 end; match perform Exc.raise(1) do x -> x | effect Exc.raise n -> n | effect Exc.raise n -> n end end");
    Alcotest.test_case "resume outside effect branch rejected" `Quick
      (elab_fail "resume 1");
    Alcotest.test_case "resume without argument rejected" `Quick
      (fun () ->
        match Parse_expand.parse_expr "resume" with
        | exception _ -> ()
        | _ -> Alcotest.fail "expected parse failure");
    Alcotest.test_case "full multi-operation handler removes effect" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S; put : S -> Unit end; \
          (fn(_) -> match perform State.get () do x -> x | effect State.get () -> 0 | effect State.put next -> resume() end : Unit -> I64) end");
    Alcotest.test_case "parameterized handlers distinguish effect instances" `Quick
      (elab_ok
         "do effect State(S) = sig get : Unit -> S end; \
          StateI64 = State(I64); \
          StateBool = State(Bool); \
          (fn(_) -> \
            match (if perform StateBool.get () do perform StateI64.get () else 0 end) do \
              x -> x \
            | effect StateI64.get () -> resume 1 \
            | effect StateBool.get () -> resume true \
            end \
           : Unit -> I64) end");
    Alcotest.test_case "parameterized handler remains effectful when instance missing" `Quick
      (elab_fail
         "do effect State(S) = sig get : Unit -> S end; \
          StateI64 = State(I64); \
          StateBool = State(Bool); \
          (fn(_) -> \
            match (if perform StateBool.get () do perform StateI64.get () else 0 end) do \
              x -> x \
            | effect StateI64.get () -> resume(1) \
            end \
           : Unit -> I64) end");
    Alcotest.test_case "handler branch can perform handled effect" `Quick
      (elab_ok
         "do effect Ping = sig hit : I64 -> I64 end; \
          (match perform Ping.hit(1) do x -> x | effect Ping.hit n -> perform Ping.hit(n + 1) end : I64) end");
  ]

let imports =
  [
    Alcotest.test_case "basic import" `Quick
      (check_import_type [ ("math", "pub x = 41; pub y = x + 1") ]
         "do M = import \"math\"; M.y end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "private import member hidden" `Quick
      (import_elab_fail [ ("m", "secret = 1; pub exposed = secret + 1") ]
         "do M = import \"m\"; M.secret end");
    Alcotest.test_case "private import member usable internally" `Quick
      (check_import_type [ ("m", "secret = 1; pub exposed = secret + 1") ]
         "do M = import \"m\"; M.exposed end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "nested import" `Quick
      (check_import_type [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ]
         "do W = import \"wrapper\"; W.M.x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes public value" `Quick
      (check_import_type [ ("math", "pub x = 42") ]
         "do M = import \"math\"; open M; x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module hides private value" `Quick
      (import_elab_fail [ ("m", "secret = 1; pub exposed = 2") ]
         "do M = import \"m\"; open M; secret end");
    Alcotest.test_case "open imported nested module" `Quick
      (check_import_type [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ]
         "do W = import \"wrapper\"; open W; open M; x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module does not re-export" `Quick
      (import_elab_fail
         [ ("base", "pub x = 41"); ("wrapper", "B = import \"base\"; pub y = do open B; x + 1 end") ]
         "do W = import \"wrapper\"; W.x end");
    Alcotest.test_case "imported ADT match" `Quick
      (check_import_type
         [ ("color", "pub type Color = Red | Green | Blue; pub default = Green") ]
         "do C = import \"color\"; match C.default do C.Red -> 1 | C.Green -> 2 | C.Blue -> 3 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes constructors" `Quick
      (check_import_type [ ("color", "pub type Color = Red | Green") ]
         "do C = import \"color\"; open C; match Red do Red -> 1 | Green -> 2 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module hides private constructors" `Quick
      (import_elab_fail [ ("secret", "type Hidden = Wrap I64; pub value = Wrap(1)") ]
         "do S = import \"secret\"; open S; match value do Wrap(n) -> n end end");
    Alcotest.test_case "repeated import" `Quick
      (check_import_type [ ("m", "pub x = 21") ]
         "do A = import \"m\"; B = import \"m\"; A.x + B.x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record field access" `Quick
      (check_import_type [ ("shapes", "pub type Point = {x: I64; y: I64}") ]
         "do S = import \"shapes\"; (S.Point{x = 1; y = 2}).x end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern" `Quick
      (check_import_type [ ("shapes", "pub type Point = {x: I64; y: I64}") ]
         "do S = import \"shapes\"; match S.Point{x = 1; y = 2} do S.Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern alias" `Quick
      (check_import_type [ ("shapes", "pub type Point = {x: I64; y: I64}") ]
         "do S = import \"shapes\"; Alias = S; match S.Point{x = 1; y = 2} do Alias.Point {x; y} -> x + y end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported method uses self" `Quick
      (check_import_type
         [ ("box", "pub Box = struct value: I64; pub method get() -> self.value end") ]
         "do B = import \"box\"; B.Box.get(B.Box{value = 1}) end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported method uses Self" `Quick
      (check_import_type
         [ ("box", "pub Box = struct value: I64; pub method copy(other : Self) -> other.value end") ]
         "do B = import \"box\"; B.Box.copy(B.Box{value = 1})(B.Box{value = 2}) end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported nested constructor pattern" `Quick
      (check_import_type
         [ ("nested", "pub M = module pub type T = X(I64) | Y end") ]
         "do N = import \"nested\"; match N.M.X(7) do N.M.X(n) -> n | N.M.Y -> 0 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported module alias pattern" `Quick
      (check_import_type [ ("color", "pub type Color = Red | Green") ]
         "do C = import \"color\"; Alias = C; match C.Red do Alias.Red -> 1 | Alias.Green -> 2 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "wrong-nominal imported constructor pattern" `Quick
      (import_elab_fail
         [ ("a", "pub type Color = Red"); ("b", "pub type Color = Red") ]
         "do A = import \"a\"; B = import \"b\"; match A.Red do B.Red -> 1 | _ -> 0 end end");
    Alcotest.test_case "imported public effect handler" `Quick
      (check_import_type [ ("effects", "pub effect Exc = sig raise : I64 -> I64 end") ]
         "do E = import \"effects\"; match perform E.Exc.raise 1 do x -> x | effect E.Exc.raise n -> n + 1 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes effect" `Quick
      (check_import_type [ ("effects", "pub effect Exc = sig raise : I64 -> I64 end") ]
         "do E = import \"effects\"; open E; match perform Exc.raise 1 do x -> x | effect Exc.raise n -> n + 1 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported public parameterized effect handler" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig get : Unit -> S end") ]
         "do E = import \"effects\"; \
           StateI64 = E.State(I64); \
           match perform StateI64.get() do x -> x | effect StateI64.get () -> 42 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported private effect hidden" `Quick
      (import_elab_fail
         [ ( "effects",
              "effect Hidden(S) = sig get : Unit -> S end; \
               pub read : Unit -> I64 can Hidden(I64) = fn(_) -> perform Hidden.get()" ) ]
         "do E = import \"effects\"; \
           match E.read() do x -> x | effect E.Hidden.get () -> 0 end end");
    Alcotest.test_case "imported latent effect function" `Quick
      (check_import_type
         [ ( "effects",
              "pub effect State(S) = sig get : Unit -> S end; \
               pub read : Unit -> I64 can State(I64) = fn(_) -> perform State.get()" ) ]
         "do E = import \"effects\"; \
           StateI64 = E.State(I64); \
           match E.read() do x -> x | effect StateI64.get () -> 7 end end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported parameterized handler distinguishes instances" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig get : Unit -> S end") ]
         "do E = import \"effects\"; \
           StateI64 = E.State(I64); \
           StateBool = E.State(Bool); \
          (fn(_) -> \
             match (if perform StateBool.get() do perform StateI64.get() else 0 end) do \
              x -> x \
            | effect StateI64.get () -> resume(1) \
            | effect StateBool.get () -> resume(true) \
            end \
           : Unit -> I64) end"
         (Pi { explicitness = Explicit; domain = AtomTy Atom_ty.TUnit; effects = empty_effect_row; codomain = AtomTy Atom_ty.TI64 }));
    Alcotest.test_case "imported private constructor hidden" `Quick
      (import_elab_fail [ ("secret", "type Hidden = Wrap I64; pub value = Wrap(1)") ]
         "do S = import \"secret\"; match S.value do S.Wrap(n) -> n end end");
    Alcotest.test_case "imported private member hidden through alias" `Quick
      (import_elab_fail [ ("m", "secret = 1; pub exposed = 2") ]
         "do M = import \"m\"; Alias = M; Alias.secret end");
    Alcotest.test_case "missing import" `Quick
      (fun () ->
        let loader = Core_loader.create ~base_dir:(Filename.temp_dir "fun_core_test" "") in
        match elab_with_loader loader "import \"missing\"" with
        | exception Core_loader.ImportNotFound "missing" -> ()
        | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
        | _ -> Alcotest.fail "expected missing import");
    Alcotest.test_case "circular import" `Quick
      (fun () ->
        with_modules [ ("a", "pub B = import \"b\""); ("b", "pub A = import \"a\"") ]
          (fun loader ->
            match elab_with_loader loader "import \"a\"" with
            | exception Core_loader.CircularImport "a" -> ()
            | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
            | _ -> Alcotest.fail "expected circular import"));
    Alcotest.test_case "import requires loader" `Quick
      (fun () ->
        match elab "import \"m\"" with
        | exception Elaborate.ElabError (Elaborate.ImportRequiresLoader "m") -> ()
        | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
        | _ -> Alcotest.fail "expected import loader error");
  ]

let references =
  [
    Alcotest.test_case "Ref I64 is Type" `Quick (check_type "Ref(I64)" U);
    Alcotest.test_case "ref annotation" `Quick (elab_ok "(ref(1) : Ref(I64))");
    Alcotest.test_case "deref type" `Quick (check_type "do r = ref(1); deref(r) end" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "assignment type" `Quick (check_type "do r = ref(1); r <- 2 end" (AtomTy Atom_ty.TUnit));
    Alcotest.test_case "wrong assignment rejected" `Quick (elab_fail "do r = ref(1); r <- true end");
    Alcotest.test_case "deref non-ref rejected" `Quick (elab_fail "deref(1)");
    Alcotest.test_case "assign non-ref rejected" `Quick (elab_fail "1 <- 2");
  ]

let let_rec =
  [
    Alcotest.test_case "recursive function annotated" `Quick
      (check_type
         "do rec f : I64 -> I64 = fn(n) -> if n == 0 do 0 else n + f(n - 1) end; f(5) end"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "recursive identity" `Quick
      (check_type
         "do rec f : I64 -> I64 = fn(n) -> if n == 0 do n else f(n - 1) end; f end"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
    Alcotest.test_case "recursive bool" `Quick
      (check_type
         "do rec f : I64 -> Bool = fn(n) -> if n == 0 do true else f(n - 1) end; f(3) end"
         (AtomTy Atom_ty.TBool));
  ]

let () =
  Alcotest.run "elaborate"
    [
      ("constants", constants);
      ("let_bindings", let_bindings);
      ("conditionals", conditionals);
      ("lambdas", lambdas);
      ("annotations", annotations);
      ("tuples", tuples);
      ("operators", operators);
      ("equality_rejections", equality_rejections);
      ("dependent", dependent);
      ("meta_solving", meta_solving);
      ("structs", structs);
      ("functors", functors);
      ("tuple_proj", tuple_proj);
      ("adts", adts);
      ("match", match_tests);
      ("implicit_args", implicit_args);
      ("traits", traits);
      ("effects", effects);
      ("imports", imports);
      ("references", references);
      ("let_rec", let_rec);
    ]
