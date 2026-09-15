open Core

let () =
  Printexc.register_printer (function
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

let builtin_syntax = Lazy.force Elab_prelude.stdlib_syntax_exports
let parse_expr source = Parse_expand.parse_expr ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source
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
  let loader = Core_loader.create ~base_dir:dir ~builtin_syntax () in
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
          | AtomTy Atom_ty.TUnit -> "Unit"
          | Pi _ -> "<pi>"
          | ProdTy _ -> "<prod>"
          | _ -> "<other>"))

let check_type_src source expected_src () =
  let ctx = Elaborate.init_ctx () in
  let _core, ty = Elaborate.on_expr ctx (parse_expr source) in
  let ecore, _ = Elaborate.on_expr ctx (parse_expr expected_src) in
  let expected_val = Elaborate.Ctx.eval ctx ecore in
  if not (Nbe.conv ctx.metas 0 ty expected_val) then
    Alcotest.fail (Printf.sprintf "type mismatch for %s: %s vs %s" source (Debug.pp_value_short ctx.metas ty) (Debug.pp_value_short ctx.metas expected_val))

(* Compares the type of [source] against the type of a reference expression
   [ref_src]. Useful when the expected type (e.g. a product involving the
   nominal [Bool]) cannot be written as a plain type-valued expression. *)
let check_type_of source ref_src () =
  let ctx = Elaborate.init_ctx () in
  let _c1, ty = Elaborate.on_expr ctx (parse_expr source) in
  let _c2, ref_ty = Elaborate.on_expr ctx (parse_expr ref_src) in
  if not (Nbe.conv ctx.metas 0 ty ref_ty) then
    Alcotest.fail (Printf.sprintf "type mismatch for %s" source)

let elab_ok source () =
  let _core, _ty = elab source in
  ()

let eval_i64 source expected () =
  let ctx = Elaborate.init_ctx () in
  let core, _ = Elaborate.on_expr ctx (parse_expr source) in
  match Elaborate.Ctx.eval ctx core with
  | VAtom (I64 n) -> Alcotest.(check int64) source expected n
  | _ -> Alcotest.fail ("expected an I64: " ^ source)

let elab_fail source () =
  match elab source with
  | exception Elaborate.ElabError _ -> ()
  | exception Unify.UnifyError _ -> ()
  | _ -> Alcotest.fail "expected elaboration error"

let constants =
  [
    Alcotest.test_case "int" `Quick (check_type "42" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "unit" `Quick (check_type "()" (AtomTy Atom_ty.TUnit));
    Alcotest.test_case "True" `Quick (check_type_src "True" "Bool");
    Alcotest.test_case "False" `Quick (check_type_src "False" "Bool");
    Alcotest.test_case "char" `Quick (check_type "'a'" (AtomTy Atom_ty.TChar));
    Alcotest.test_case "string" `Quick (check_type "\"hello\"" (AtomTy Atom_ty.TString));
    Alcotest.test_case "string type" `Quick (check_type "String" U);
    Alcotest.test_case "absurd type" `Quick (check_type "Absurd" U);
  ]

let let_bindings =
  [
    Alcotest.test_case "simple let" `Quick
      (check_type "{ x = 1; x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "let bool" `Quick
      (check_type_src "{ b = True; b }" "Bool");
    Alcotest.test_case "let shadowing" `Quick
      (check_type "{ x = True; x = 1; x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "lambda shadows outer let" `Quick
      (check_type "{ x = True; (fn(x) { x } : I64 -> I64)(1) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "non-rec let rhs sees outer binding" `Quick
      (check_type "{ x = 1; x = x; x }" (AtomTy Atom_ty.TI64));
  ]

let conditionals =
  [
    Alcotest.test_case "simple if" `Quick
      (check_type "if (True) { 1 } else { 2 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "if branches must match" `Quick
      (elab_fail "if (True) { 1 } else { False }");
    Alcotest.test_case "nested if" `Quick
      (check_type "if (True) { if (False) { 1 } else { 2 } } else { 3 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "if branches are blocks" `Quick
      (check_type "if (True) { y = 1; y + 1 } else { 3 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "if branches require braces" `Quick
      (fun () ->
        match elab "if (True) 1 else 2" with
        | exception Enforest_util.Error msg when String.starts_with ~prefix:"no matching branch for syntax if" msg -> ()
        | _ -> Alcotest.fail "expected unbraced if branches to be rejected");
  ]

let lambdas =
  [
    Alcotest.test_case "application" `Quick
      (check_type "((fn(x) { x }) : I64 -> I64)(42)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "apply non-function" `Quick
      (elab_fail "1(2)");
    Alcotest.test_case "annotated identity" `Quick
      (check_type "(fn(x) { x } : I64 -> I64)"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
    Alcotest.test_case "bool function" `Quick
      (check_type_src "(fn(x) { x } : Bool -> Bool)" "Bool -> Bool");
    Alcotest.test_case "higher-order twice" `Quick
      (check_type
         "{ twice : (I64 -> I64) -> I64 -> I64 = fn(f, x) { f(f(x)) }; twice }"
         (pi Explicit
            (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64))
            (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64))));
  ]

let annotations =
  [
    Alcotest.test_case "int annotation" `Quick
      (check_type "(42 : I64)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "bool annotation" `Quick
      (check_type_src "(True : Bool)" "Bool");
    Alcotest.test_case "char annotation" `Quick
      (check_type "('a' : Char)" (AtomTy Atom_ty.TChar));
    Alcotest.test_case "let with annotation" `Quick
      (check_type "{ x : I64 = 42; x }" (AtomTy Atom_ty.TI64));
  ]

let tuples =
  [
    Alcotest.test_case "pair" `Quick
      (check_type_of "(1, True)" "((0, False) : I64 * Bool)");
    Alcotest.test_case "triple" `Quick
      (check_type "(1, 2, 3)"
         (ProdTy [ AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64 ]));
  ]

let operators =
  [
    Alcotest.test_case "add" `Quick (check_type "1 + 2" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "compare" `Quick (check_type_src "1 == 2" "Bool");
    Alcotest.test_case "bool equality" `Quick (check_type_src "True == False" "Bool");
    Alcotest.test_case "char equality" `Quick (check_type_src "'a' == 'a'" "Bool");
    Alcotest.test_case "unit equality" `Quick (check_type_src "() == ()" "Bool");
    Alcotest.test_case "type alias equality" `Quick
      (check_type_src "{ MyInt = I64; (1 : MyInt) == (2 : MyInt) }" "Bool");
    Alcotest.test_case "builtin alias chain equality" `Quick
      (check_type_src "{ MyInt = I64; Alias = MyInt; (1 : Alias) == (2 : MyInt) }" "Bool");
    Alcotest.test_case "operator as value" `Quick (check_type_src "(==)(1)(1)" "Bool");
    Alcotest.test_case "polymorphic helper" `Quick
      (check_type_src "{ same : [A : Type] -> A -> A -> Bool = fn[A : Type] { (==)[A] }; same(1, 1) }" "Bool");
    Alcotest.test_case "complex" `Quick
      (check_type "{ x = 1 + 2; x * 3 }" (AtomTy Atom_ty.TI64));
  ]

let equality_rejections =
  [
    Alcotest.test_case "mismatch rejection" `Quick (elab_fail "1 == True");
    Alcotest.test_case "nominal equality requires impl" `Quick
      (elab_fail "{ type Color = Red; Red == Red }");
    Alcotest.test_case "record equality requires impl" `Quick
      (elab_fail "{ type Point = struct {x: I64}; Point{x = 1} == Point{x = 1} }");
  ]

let dependent =
  [
    Alcotest.test_case "Type as value" `Quick
      (check_type "(I64 : Type)" U);
    Alcotest.test_case "type-head match I64" `Quick
      (check_type "match (I64) { I64 => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Bool" `Quick
      (check_type "match (Bool) { Bool => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Char" `Quick
      (check_type "match (Char) { Char => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-head match Unit" `Quick
      (check_type "match (Unit) { Unit => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open Type match fallback" `Quick
      (check_type "{ classify : Type -> I64 = fn(T) { match (T) { I64 => 1, _ => 0 } }; classify(Bool) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "type-case refines dependent argument" `Quick
      (check_type_src
         "{ is_zeroish : [T : Type] -> T -> Bool = fn[T : Type](x) { \
          match (T) { \
          I64 => x == 0, \
          Bool => x == False, \
          Unit => True, \
          Char => x == 'a', \
          _ => False \
          } }; is_zeroish }"
         "[T : Type] -> T -> Bool");
    Alcotest.test_case "type-case rejects invalid refined branch" `Quick
      (elab_fail
         "{ bad : (T : Type) -> T -> Bool = fn(T : Type, x) { \
          match (T) { \
          I64 => x == False, \
          _ => False \
          } }; bad }");
    Alcotest.test_case "type-case default refines return type" `Quick
      (check_type
         "{ default : [T : Type] -> T = fn[T : Type] { \
          match (T) { \
          I64 => 0, \
          Bool => False, \
          Unit => (), \
          Char => 'a', \
           _ => panic(\"no default\") \
          } }; default }"
         (Pi { explicitness = Implicit; domain = U; effects = empty_effect_row; codomain = Var 0 }));
    Alcotest.test_case "type-case default with fallback" `Quick
      (elab_ok
         "{ default_or : [T : Type] -> T -> T = fn[T : Type](fallback) { \
          match (T) { \
          I64 => 0, \
          Bool => False, \
          Unit => (), \
          Char => 'a', \
          String => \"\", \
          _ => fallback \
          } }; default_or }");
    Alcotest.test_case "type-case string classifier" `Quick
      (elab_ok
         "{ type_name : Type -> String = fn(T) { \
          match (T) { \
          I64 => \"i64\", \
          Bool => \"bool\", \
          Char => \"char\", \
          Unit => \"unit\", \
          String => \"string\", \
          _ => \"other\" \
          } }; type_name }");
    Alcotest.test_case "type-case nominal classifier" `Quick
      (elab_ok
         "{ type Option a = Some a | None; \
          classify : Type -> I64 = fn(T) { \
          match (T) { \
          Option(I64) => 1, \
          Option _ => 2, \
          _ => 0 \
          } }; classify }");
    Alcotest.test_case "type-case struct field type binder" `Quick
      (elab_ok
         "{ classify : Type -> I64 = fn(T) { \
          match (T) { \
          struct { x: p; _ } => match (p) { I64 => 1, _ => 2 }, \
          _ => 0 \
          } }; classify }");
    Alcotest.test_case "type-case struct field type pattern" `Quick
      (elab_ok
         "{ classify : Type -> I64 = fn(T) { \
          match (T) { \
          struct { x: I64; _ } => 1, \
          struct { x: Bool; _ } => 2, \
          _ => 0 \
          } }; classify }");
    Alcotest.test_case "type-case duplicate struct field pattern rejects" `Quick
      (elab_fail
         "match (I64) { \
          struct { x: I64; x: Bool; _ } => 1, \
          _ => 0 \
          }");
    Alcotest.test_case "type-case struct pattern on non-Type rejects" `Quick
      (elab_fail "match (1) { struct { x: I64; _ } => 1, _ => 0 }");
    Alcotest.test_case "type-case unresolved uppercase pattern rejects" `Quick
      (elab_fail
         "{ type Option a = Some a | None; \
          match (Option(I64)) { Option X => I64, _ => Bool } }");
    Alcotest.test_case "type-passing identity" `Quick
      (elab_ok
         "((fn(T : Type, x : T) { x }) : Type -> I64 -> I64)(I64)(42)");
    Alcotest.test_case "type-level if" `Quick
      (elab_ok
         "{ choose : Type -> Type -> Bool -> Type = fn(a, b, c) { if (c) { a } else { b } }; (42 : choose(I64, Bool, True)) }");
    Alcotest.test_case "dependent return type" `Quick
      (elab_ok
         "{ f : Bool -> Type = fn(b) { if (b) { I64 } else { Bool } }; (42 : f(True)) }");
    Alcotest.test_case "dependent return type False" `Quick
      (elab_ok
         "{ f : Bool -> Type = fn(b) { if (b) { I64 } else { Bool } }; (True : f(False)) }");
    Alcotest.test_case "dependent mismatch" `Quick
      (elab_fail
         "{ f : Bool -> Type = fn(b) { if (b) { I64 } else { Bool } }; (True : f(True)) }");
  ]

let meta_solving =
  [
    Alcotest.test_case "infer identity arg" `Quick
      (check_type "((fn(x) { x }) : I64 -> I64)(42)" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "infer through let" `Quick
      (check_type "{ f : I64 -> I64 = fn(x) { x }; f(42) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "infer lambda param from body" `Quick
      (check_type "(fn(x) { x + 1 } : I64 -> I64)"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
  ]

let structs =
  [
    Alcotest.test_case "empty module" `Quick
      (elab_ok "module { }");
    (* A [type] member inside a [struct] used to push one context entry too many
       (two, when parameterised), so every member after it resolved to the wrong
       de Bruijn index. See env-width-contract-is-unnamed. *)
    Alcotest.test_case "struct type member then value member" `Quick
      (check_type
         "{ S = struct { x : I64; type L = N | C(I64); pub g = 5 }; S.g }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "struct parameterised type member then value member" `Quick
      (check_type
         "{ S = struct { x : I64; type P(a) = Q(a); pub g = 7 }; S.g }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open module" `Quick
      (check_type
         "{ S = module { pub x = 42 }; open S; x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open struct constructors" `Quick
      (elab_ok
         "{ Color = module { pub type Color = Red | Green | Blue }; \
          open Color; Red }");
    Alcotest.test_case "struct with pub fields" `Quick
      (elab_ok
         "{ S = module { pub x = 1; pub y = True }; open S; if (y) { x } else { 0 } }");
    Alcotest.test_case "nested struct open" `Quick
      (check_type
         "{ Outer = module { pub Inner = module { pub val = 42 } }; open Outer; open Inner; val }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open only imports pub" `Quick
      (elab_fail
         "{ S = module { x = 42 }; open S; x }");
    Alcotest.test_case "field access" `Quick
      (check_type
         "{ S = module { pub x = 42 }; S.x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "field access boolean" `Quick
      (check_type_src
         "{ S = module { pub x = 1; pub y = True }; S.y }"
         "Bool");
    Alcotest.test_case "nested field access" `Quick
      (check_type
         "{ Outer = module { pub Inner = module { pub val = 42 } }; Outer.Inner.val }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "field not found" `Quick
      (elab_fail "{ S = module { pub x = 1 }; S.y }");
    Alcotest.test_case "private not accessible" `Quick
      (elab_fail "{ S = module { x = 42 }; S.x }");
    Alcotest.test_case "field decls" `Quick
      (elab_ok "struct { x: I64; y: Bool; }");
    Alcotest.test_case "field decls with pub binding" `Quick
      (elab_ok "struct { x: I64; pub fourty_two = 42 }");
    Alcotest.test_case "pass record through function" `Quick
      (elab_ok
         "{ Point = struct { x: I64; }; (fn(p) { p.x })(Point{x = 42}) }");
    Alcotest.test_case "module signature argument" `Quick
      (check_type
         "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 1 })"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "module signature allows extra fields" `Quick
      (check_type
         "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 1; pub y = True })"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "signature sugar argument" `Quick
      (check_type
         "(fn(m : sig { x : I64 }) { m.x })(module { pub x = 1 })"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open a module parameter" `Quick
      (eval_i64 "(fn(m : sig { x : I64 }) { open m; x + 1 })(module { pub x = 41 })" 42L);
    Alcotest.test_case "open a module parameter with more members than its signature" `Quick
      (eval_i64 "(fn(m : sig { x : I64 }) { open m; x + 1 })(module { pub y = 5; pub x = 41 })" 42L);
    Alcotest.test_case "open a module parameter in a nested lambda" `Quick
      (eval_i64 "(fn(m : sig { x : I64 }) { fn(u : I64) { open m; x + u } })(module { pub x = 41 })(1)" 42L);
    Alcotest.test_case "open a module parameter whose signature has a type member" `Quick
      (eval_i64 "(fn(m : sig { T : Type; v : I64 }) { open m; (v : I64) })(module { pub T = Bool; pub v = 3 })" 3L);
    Alcotest.test_case "module signature missing field rejected" `Quick
      (elab_fail
         "(fn(m : sig { x : I64 }) { m.x })(module { pub y = 1 })");
    Alcotest.test_case "module signature wrong field type rejected" `Quick
      (elab_fail
         "(fn(m : sig { x : I64 }) { m.x })(module { pub x = True })");
    Alcotest.test_case "module signature private field rejected" `Quick
      (elab_fail
         "(fn(m : sig { x : I64 }) { m.x })(module { x = 1 })");
    Alcotest.test_case "record struct does not satisfy module signature" `Quick
      (elab_fail
         "{ Point = struct { x: I64; }; (fn(m : sig { x : I64 }) { m.x })(Point) }");
    Alcotest.test_case "private used by pub" `Quick
      (check_type
         "{ S = module { helper = 42; pub x = helper }; S.x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record construction" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; Point{x = 1; y = 2} }"
         (Struct { con_fields = [ ("x", AtomTy Atom_ty.TI64); ("y", AtomTy Atom_ty.TI64) ]; bindings = []; partial = false }));
    Alcotest.test_case "record field access" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; (Point{x = 1; y = 2}).x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized record construction" `Quick
      (elab_ok
         "{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; } }; (Pair[I64, Bool]{fst = 1; snd = True}).snd }");
    Alcotest.test_case "record type declaration" `Quick
      (check_type
         "{ type Point = struct {x: I64; y: I64}; (Point{x = 1; y = 2}).x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized record type declaration" `Quick
      (check_type_src
         "{ type Pair A B = struct {fst: A; snd: B}; (Pair{fst = 1; snd = True}).snd }"
         "Bool");
    Alcotest.test_case "record type declaration pattern" `Quick
      (check_type
         "{ type Point = struct {x: I64; y: I64}; match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record construction field order" `Quick
      (check_type
         "{ type Point = struct {x: I64; y: I64}; p = Point{y = 20; x = 10}; p.x + p.y }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "polymorphic record multiple instantiations" `Quick
      (check_type
         "{ type Pair A B = struct {fst: A; snd: B}; \
          p1 = Pair{fst = 10; snd = 20}; \
          p2 = Pair{fst = True; snd = 3}; \
          if (p2.fst) { p1.fst + p2.snd } else { 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record type declaration missing field" `Quick
      (elab_fail "{ type Point = struct {x: I64; y: I64}; Point{x = 1} }");
    Alcotest.test_case "record type declaration unknown field" `Quick
      (elab_fail "{ type Point = struct {x: I64}; Point{x = 1; y = 2} }");
    Alcotest.test_case "record construction duplicate field" `Quick
      (elab_fail "{ type Point = struct {x: I64}; Point{x = 1; x = 2} }");
    Alcotest.test_case "record declaration duplicate field" `Quick
      (elab_fail "{ type Point = struct {x: I64; x: Bool}; Point }");
    Alcotest.test_case "record type declaration same recursion" `Quick
      (elab_ok "{ type Option A = Some A | None; type List A = struct {meta: A; next: Option(List(A))}; List }");
    Alcotest.test_case "recursive record construction" `Quick
      (elab_ok "{ type Option A = Some A | None; type List A = struct {meta: A; next: Option(List(A))}; List{meta = 1; next = None} }");
    Alcotest.test_case "recursive record rejects non-self payload" `Quick
      (elab_fail "{ type Option A = Some A | None; type List A = struct {meta: A; next: Option(List(A))}; List{meta = 1; next = Some(2)} }");
    Alcotest.test_case "record rewrite respects type name shadowing" `Quick
      (elab_ok "{ type R A = struct {x: (fn(R) { R })(I64)}; R }");
    Alcotest.test_case "record rewrite respects parameter shadowing" `Quick
      (elab_ok "{ type R A = struct {x: (fn(A) { A })(I64)}; R }");
    Alcotest.test_case "record type declaration changed recursion rejected" `Quick
      (elab_fail "{ type Bad(A, B) = struct {x: Bad(B, A)}; Bad }");
    Alcotest.test_case "method uses self" `Quick
      (check_type
         "{ Box = fn[A : Type] { struct { value: A; pub method get() { self.value } } }; Box[I64].get(Box[I64]{value = 1}) }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized method uses self" `Quick
      (check_type_src
         "{ Pair = fn[A : Type, B : Type] { struct { fst: A; snd: B; pub method swap() { (self.snd, self.fst) } } }; (Pair[I64, Bool].swap(Pair[I64, Bool]{fst = 1; snd = True})).0 }"
         "Bool");
    Alcotest.test_case "method extra parameter" `Quick
      (check_type
         "{ Counter = struct { value: I64; pub method add(x) { self.value + x } }; Counter.add(Counter{value = 1})(2) }"
         (AtomTy Atom_ty.TI64));
    (* A NomRef names the template; a binding holding an instance of the same
       nominal ([Decls = List(Decl)] in the prelude) must not be taken for it. *)
    Alcotest.test_case "nominal reference skips an instance binding" `Quick
      (elab_ok "{ f = fn(d : List(I64)) { d }; g = fn(d : List(I64)) { x : List(I64) = f(d); x }; 1 }");
    Alcotest.test_case "nominal reference skips a local instance binding" `Quick
      (elab_ok "{ type Box A = MkBox(A); BoolBox = Box(Bool); f = fn(b : Box(I64)) { b }; g = fn(b : Box(I64)) { fn(u : Unit) { y : Box(I64) = f(b); y } }; 1 }");
    Alcotest.test_case "nominal reference skips an option instance" `Quick
      (elab_ok "{ OB = Option(Bool); f = fn(o : Option(I64)) { o }; g = fn(o : Option(I64)) { y : Option(I64) = f(o); y }; 1 }");
    Alcotest.test_case "method uses Self type" `Quick
      (check_type
         "{ Box = fn[A : Type] { struct { value: A; pub method id(other : Self) { other.value } } }; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "Self in let binding" `Quick
      (elab_ok
         "{ Box = struct { value: I64; pub id = fn(b : Self) { b.value } }; Box.id(Box{value = 1}) }");
    Alcotest.test_case "self outside method" `Quick
      (elab_fail "self");
    Alcotest.test_case "self in let binding" `Quick
      (elab_fail "{ Box = struct { value: I64; pub bad = self.value }; Box.bad }");
    Alcotest.test_case "opening a non-module is an error in every open form" `Quick (fun () ->
      List.iter
        (fun source ->
          match elab source with
          | exception Elaborate.ElabError NotAModule -> ()
          | exception e -> Alcotest.fail (source ^ ": " ^ Printexc.to_string e)
          | _ -> Alcotest.fail (source ^ ": expected NotAModule"))
        [ "{ open 5; 1 }"; "(fn(x : I64) { open x; x })(1)"; "module { open 5; pub y = 1 }" ]);
    Alcotest.test_case "a field type sees an earlier open" `Quick
      (check_type_src "{ M = module { pub T = I64 }; R = struct { open M; f : T }; R{f = 1}.f }" "I64");
    Alcotest.test_case "a field type sees an earlier binding" `Quick
      (check_type_src "{ R = struct { pub T = I64; f : T }; R{f = 1}.f }" "I64");
    Alcotest.test_case "an earlier open shadows an outer name in a field type" `Quick
      (check_type_src "{ T = Bool; M = module { pub T = I64 }; R = struct { open M; f : T }; R{f = 1}.f }" "I64");
    Alcotest.test_case "one name means one thing in a struct" `Quick
      (check_type_src "{ T = I64; M = module { pub T = Bool }; R = struct { open M; f : T; pub g : T = True }; R{f = False}.f }" "Bool");
    Alcotest.test_case "a field type does not see a later binding" `Quick
      (elab_fail "{ R = struct { f : T; pub T = I64 }; R{f = 1}.f }");
    Alcotest.test_case "a field type does not see another field" `Quick
      (elab_fail "{ R = struct { n : Type; v : n }; 0 }");
    Alcotest.test_case "a method sees a later field" `Quick
      (check_type_src "{ C = struct { a : I64; pub method get() { self.b }; b : I64 }; C.get(C{a = 1; b = 9}) }" "I64");
    Alcotest.test_case "a field type mentioning an earlier method is a cycle" `Quick
      (fun () ->
        match elab "{ C = struct { a : I64; pub method get() { self.a }; b : get; }; 0 }" with
        | exception Elaborate.ElabError (FieldTypeMentionsMethod { field = "b"; _ }) -> ()
        | exception e -> Alcotest.fail (Printexc.to_string e)
        | _ -> Alcotest.fail "expected a field/method cycle error");
    Alcotest.test_case "a struct does not see its own name" `Quick
      (elab_fail "{ C = struct { pub k = 1; pub method get() { self.value + C.k }; value : I64 }; 0 }");
    Alcotest.test_case "an outer binding of the struct's name is still visible" `Quick
      (check_type_src "{ C = 1; C = struct { pub k = C }; C.k }" "I64");
    Alcotest.test_case "record construction missing field" `Quick
      (elab_fail "{ Point = struct { x: I64; y: I64; }; Point{x = 1} }");
    Alcotest.test_case "record construction unknown field" `Quick
      (elab_fail "{ Point = struct { x: I64; }; Point{x = 1; y = 2} }");
    Alcotest.test_case "record construction duplicate field" `Quick
      (elab_fail "{ Point = struct { x: I64; }; Point{x = 1; x = 2} }");
  ]

let functors =
  [
    Alcotest.test_case "identity functor" `Quick
      (check_type
         "{ Double = fn(M : sig { x : I64 }) { module { pub doubled = M.x + M.x } }; (Double(module { pub x = 21 })).doubled }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "functor pass through" `Quick
      (elab_ok
         "{ F = fn(M : sig { x : I64 }) { module { pub y = M.x } }; A = module { pub x = 1 }; B = F(A); B.y }");
    Alcotest.test_case "functor with private helper" `Quick
      (check_type
         "{ F = fn(M : sig { x : I64 }) { module { tmp = M.x; pub y = tmp + 1 } }; (F(module { pub x = 1 })).y }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "compose functors" `Quick
      (elab_ok
         "{ F = fn(M : sig { x : I64 }) { module { pub a = M.x } }; G = fn(N : sig { a : I64 }) { module { pub b = N.a } }; (G(F(module { pub x = 1 }))).b }");
    Alcotest.test_case "higher order functor" `Quick
      (elab_ok
         "{ Apply = fn(F, M : sig { x : I64 }) { F(M) }; Apply(fn(M : sig { x : I64 }) { module { pub z = M.x } })(module { pub x = 1 }) }");
  ]

let tuple_proj =
  [
    Alcotest.test_case "proj first" `Quick
      (check_type "(1, True).0" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj second" `Quick
      (check_type_src "(1, True).1" "Bool");
    Alcotest.test_case "proj triple" `Quick
      (check_type "(1, 2, 3).2" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj chain" `Quick
      (check_type_src "((1, True), 42).0.1" "Bool");
    Alcotest.test_case "proj from let" `Quick
      (check_type "{ p = (1, True); p.0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "proj type error" `Quick
      (elab_fail "42.0");
  ]

let adts =
  [
    Alcotest.test_case "constructor type" `Quick (fun () ->
      let _core, ty = elab "{ type Color = Red | Green | Blue; Red }" in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "constructor should have type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "multiple constructors" `Quick (fun () ->
      (* All constructors of the same ADT have the same nominal type *)
      let _core, ty =
        elab "{ type Color = Red | Green | Blue; \
              _ : Color = Red; \
              _ : Color = Green; Color }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU");
    Alcotest.test_case "shadowing" `Quick (fun () ->
      let _core, ty =
        elab
          "{ type A = X | Y; \
           type B = X | Z; X }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B (most recent)"
      | _ -> Alcotest.fail "expected nominal");
    Alcotest.test_case "undefined constructor" `Quick
      (elab_fail "{ type Color = Red | Green; Blue }");
    Alcotest.test_case "type Color accessible in body" `Quick (fun () ->
      let _core, ty = elab "{ type Color = Red | Green | Blue; Color }" in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU (Type)");
    Alcotest.test_case "pub type inside struct" `Quick (fun () ->
      let _core, ty =
        elab
          "{ S = module { \
           pub type Color = Red | Green | Blue \
           }; S.Color }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "S.Color should have type VU (Type)");
    Alcotest.test_case "pub type ctor via dot" `Quick (fun () ->
      let _core, ty =
        elab
          "{ S = module { \
           pub type Color = Red | Green | Blue \
           }; S.Red }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "S.Red should have nominal type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "private type not visible via dot" `Quick
      (elab_fail
         "{ S = module { \
          type Color = Red | Green | Blue \
          }; S.Red }");
    Alcotest.test_case "private type visible to later binding" `Quick (fun () ->
      let _core, ty =
        elab
          "{ S = module { \
           type Color = Red | Green | Blue; \
           pub default = Red \
           }; S.default }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "private type should be usable inside struct"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two structs, distinct nominal ids" `Quick (fun () ->
      let _core, _ty =
        elab
          "{ S = module { pub type Color = Red | Green }; \
           T = module { pub type Color = Blue }; \
           _ : S.Color = S.Red; () }"
      in
      ());
    Alcotest.test_case "distinct nominal ids don't unify" `Quick
      (elab_fail
         "{ S = module { pub type Color = Red | Green }; \
          T = module { pub type Color = Blue }; \
          _ : T.Color = S.Red; () }");
    Alcotest.test_case "parameterized ADT with payload" `Quick (fun () ->
      let _core, ty =
        elab "{ type Option a = Some a | None; Some[I64](42) }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "nullary pctor has nominal type" `Quick (fun () ->
      let _core, ty =
        elab "{ type Option a = Some a | None; None[I64] }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "None I64 should have type Option(I64)"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor partial application" `Quick (fun () ->
      let _core, ty =
        elab "{ type Option a = Some a | None; Some[I64] }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VPi _ -> ()  (* Some[I64] : I64 -> Option(I64) *)
      | _ -> Alcotest.fail "Some[I64] should be a function type");
    Alcotest.test_case "multiple type params" `Quick (fun () ->
      let _core, ty =
        elab "{ type Result a e = Ok a | Err e; Ok[I64, Bool](42) }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Result") then
            Alcotest.fail "Ok[I64, Bool](42) should have type Result I64 Bool"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor via let binding" `Quick (fun () ->
      let _core, ty =
        elab "{ type Option a = Some a | None; \
              f = Some; f[I64](42) }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal via let-bound ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor passed through let" `Quick (fun () ->
      let _core, ty =
        elab "{ type Option a = Some a | None; \
              f = Some[I64]; f(42) }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option via let-bound partial ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two pctor adts, distinct types" `Quick (fun () ->
      let _core, ty =
        elab "{ type A a = X a | Y; \
              type B a = X a | Z; X[I64](42) }"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "distinct param instantiations don't unify" `Quick
      (elab_fail
         "{ type Option a = Some a | None; \
          x : Option(I64) = Some[I64](42); \
          _ : Option(Bool) = x; () }");
    Alcotest.test_case "constructor polymorphism" `Quick
      (elab_ok
         "{ type Option a = Some a | None; \
          x = Some(1); \
          y = Some(True); y }");
    Alcotest.test_case "if with ADT branches" `Quick
      (elab_ok
         "{ type Result a e = Ok a | Err e; \
          if (True) { Ok(1) } else { Err() } }");
    Alcotest.test_case "recursive parameterized ADT" `Quick
      (elab_ok
         "{ type List a = Cons(a, List(a)) | Nil; \
         Cons(1, Nil) }");
    Alcotest.test_case "recursive parameterized ADT match" `Quick
      (check_type
         "{ type List a = Cons(a, List(a)) | Nil; \
         match (Cons(1, Nil)) { Cons(x, xs) => x, Nil => 0 } }"
         (AtomTy Atom_ty.TI64));
  ]

let match_tests =
  [
    Alcotest.test_case "simple match nullary" `Quick
      (check_type
        "{ type Color = Red | Green | Blue; \
         (match (Red) { Red => 1, Green => 2, Blue => 3 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match with payload" `Quick
      (check_type
        "{ type Option a = Some a | None; \
         (match (Some[I64](42)) { Some(x) => x, None => 0 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match with wildcard" `Quick
      (check_type
        "{ type Color = Red | Green | Blue; \
         (match (Red) { Red => 1, _ => 0 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match bind variable" `Quick
      (check_type
        "{ type Color = Red | Green; \
         (match (Red) { x => 1 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "pattern binder shadows outer name" `Quick
      (check_type "{ x = True; match (1) { x => x } }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match non ADT wildcard" `Quick
      (check_type "match (42) { _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match int literal" `Quick
      (check_type "match (42) { 42 => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match bool literals" `Quick
      (check_type "match (True) { True => 1, False => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match unit literal" `Quick
      (check_type "match () { () => 1 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match char literal" `Quick
      (check_type "match ('a') { 'a' => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match escaped char literal" `Quick
      (check_type "match ('\\n') { '\\n' => 1, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "literal type mismatch" `Quick
      (elab_fail "match (1) { True => 0, _ => 1 }");
    Alcotest.test_case "char literal type mismatch" `Quick
      (elab_fail "match ('a') { 1 => 0, _ => 1 }");
    Alcotest.test_case "non-exhaustive bool literal" `Quick
      (elab_fail "match (True) { True => 1 }");
    Alcotest.test_case "non-exhaustive int literal" `Quick
      (elab_fail "match (1) { 1 => 1 }");
    Alcotest.test_case "non-exhaustive char literal" `Quick
      (elab_fail "match ('a') { 'a' => 1 }");
    Alcotest.test_case "match literal or-pattern" `Quick
      (check_type "match (1) { 0 | 1 => 42, _ => 0 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "or-pattern covers bool" `Quick
      (check_type "match (True) { True | False => 1 }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "constructor or-pattern" `Quick
      (check_type
         "{ type Color = Red | Green | Blue; \
          match (Red) { Red | Green => 1, Blue => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "constructor or-pattern binding" `Quick
      (check_type
         "{ type E = A I64 | B I64; \
          match (A(1)) { A(x) | B(x) => x } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "or-pattern binding name mismatch" `Quick
      (elab_fail
         "{ type E = A I64 | B I64; \
          match (A(1)) { A(x) | B(y) => x } }");
    Alcotest.test_case "or-pattern missing binding" `Quick
      (elab_fail
         "{ type E = A I64 | C; \
          match (A(1)) { A(x) | C => x } }");
    Alcotest.test_case "non-exhaustive constructor or-pattern" `Quick
      (elab_fail
         "{ type Color = Red | Green | Blue; \
          match (Red) { Red | Green => 1 } }");
    Alcotest.test_case "match tuple pattern" `Quick
      (check_type "match (1, True) { (x, b) => if (b) { x } else { 0 } }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "tuple or-pattern" `Quick
      (check_type
         "match (True, 1) { (True, x) | (False, x) => x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match nested tuple pattern" `Quick
      (check_type "match ((1, True), 2) { ((x, _), y) => x + y }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "tuple pattern arity mismatch" `Quick
      (elab_fail "match (1, True) { (x, y, z) => x }");
    Alcotest.test_case "tuple literal type mismatch" `Quick
      (elab_fail "match (1, True) { (True, x) => x, _ => 0 }");
    Alcotest.test_case "non-exhaustive tuple literal" `Quick
      (elab_fail "match (True, 1) { (True, x) => x }");
    Alcotest.test_case "match infers tuple scrutinee" `Quick
      (elab_ok "fn(x) { match (x) { (True, y) => y, (False, y) => y } }");
    Alcotest.test_case "match unknown constructor" `Quick
      (elab_fail "{ type Color = Red; match (Red) { Blue => 0 } }");
    Alcotest.test_case "match payload arity mismatch" `Quick
      (elab_fail "{ type Option a = Some a | None; \
                  match (Some[I64](42)) { Some => 0, None => 0 } }");
    Alcotest.test_case "match wrong scrutinee type" `Quick
      (elab_fail "{ type Color = Red | Green; match (42) { Red => 1 } }");
    Alcotest.test_case "non-exhaustive missing ctor" `Quick
      (elab_fail "{ type Color = Red | Green | Blue; \
                  match (Red) { Red => 1, Green => 2 } }");
    Alcotest.test_case "non-exhaustive nested ADT" `Quick
      (elab_fail
         "{ type C = X I64 | Y I64; \
          type B = P C | Q C; \
          type A = M B | N B; \
          match (M(P(X(1)))) { M(P(X(x))) => x } }");
    Alcotest.test_case "non-exhaustive partial nested branches" `Quick
      (elab_fail
         "{ type Inner = X I64 | Y I64; \
          type Outer = A Inner | B Inner; \
          match (A(X(1))) { A(X(x)) => x, B(X(x)) => x } }");
    Alcotest.test_case "non-exhaustive single ctor" `Quick
      (elab_fail "{ type Option a = Some a | None; \
                  match (Some(42)) { Some(x) => x } }");
    Alcotest.test_case "exhaustive with wildcard" `Quick
      (check_type
        "{ type Color = Red | Green | Blue; \
         (match (Red) { Red => 1, _ => 0 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "exhaustive all ctors" `Quick
      (check_type
        "{ type Color = Red | Green | Blue; \
         (match (Red) { Red => 1, Green => 2, Blue => 3 } : I64) }"
        (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern" `Quick
      (check_type
         "{ S = module { pub type Color = Red | Green }; \
          match (S.Red) { S.Red => 1, S.Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified nested constructor pattern" `Quick
      (check_type
         "{ A = module { pub B = module { pub type T = X I64 | Y } }; \
          match (A.B.X(7)) { A.B.X(n) => n, A.B.Y => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern alias" `Quick
      (check_type
         "{ S = module { pub type Color = Red | Green }; \
          N = S; match (S.Red) { N.Red => 1, N.Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified constructor pattern private" `Quick
      (elab_fail
         "{ S = module { type Color = Red | Green }; \
          match (S.Red) { S.Red => 1, _ => 0 } }");
    Alcotest.test_case "qualified constructor pattern wrong nominal" `Quick
      (elab_fail
         "{ S = module { pub type Color = Red }; \
           T = module { pub type Color = Red }; \
           match (S.Red) { T.Red => 1, _ => 0 } }");
    Alcotest.test_case "nested module pattern synonym constructor" `Quick
      (check_type
         "{ M = module { \
            pub B = module { pub type T = X I64 | Y }; \
            pub pattern PX(n) = B.X(n) \
          }; \
          match (M.B.X(7)) { M.PX(n) => n, M.B.Y => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "same-module pattern synonym constructor" `Quick
      (check_type
         "{ M = module { \
            pub type T = X I64 | Y; \
            pub pattern PX(n) = X(n) \
          }; \
          match (M.X(7)) { M.PX(n) => n, M.Y => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern shorthand" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; \
          match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern reordered" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; \
          match (Point{x = 1; y = 2}) { Point {y; x} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern renamed" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; \
          match (Point{x = 1; y = 2}) { Point {x = n; y} => n + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern partial" `Quick
      (check_type
         "{ Point = struct { x: I64; y: I64; }; \
          match (Point{x = 1; y = 2}) { Point {x; _} => x } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern literal dispatch" `Quick
      (check_type
         "{ Flag = struct { flag: Bool; value: I64; }; \
          match (Flag{flag = False; value = 3}) { \
          Flag {flag = True; value} => value, Flag {flag = False; value} => value + 1 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified record pattern" `Quick
      (check_type
         "{ M = module { pub Point = struct { x: I64; y: I64; } }; \
          match (M.Point{x = 1; y = 2}) { M.Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "qualified record pattern alias" `Quick
      (check_type
         "{ M = module { pub Point = struct { x: I64; y: I64; } }; \
          N = M; match (M.Point{x = 1; y = 2}) { N.Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record pattern incomplete" `Quick
      (elab_fail
         "{ Point = struct { x: I64; y: I64; }; \
          match (Point{x = 1; y = 2}) { Point {x} => x } }");
    Alcotest.test_case "record pattern unknown field" `Quick
      (elab_fail
         "{ Point = struct { x: I64; }; \
          match (Point{x = 1}) { Point {y; _} => y } }");
    Alcotest.test_case "record pattern duplicate field" `Quick
      (elab_fail
         "{ Point = struct { x: I64; }; \
          match (Point{x = 1}) { Point {x; x} => x } }");
  ]

let implicit_args =
  [
    Alcotest.test_case "implicit inference Some 42" `Quick (fun () ->
      ignore (elab "{ type Option a = Some a | None; \
                    (Some(42) : Option(I64)) }"));
    Alcotest.test_case "explicit implicit Some[I64] 42" `Quick (fun () ->
      ignore (elab "{ type Option a = Some a | None; \
                    (Some[I64](42) : Option(I64)) }"));
    Alcotest.test_case "nilary implicit None" `Quick (fun () ->
      ignore (elab "{ type Option a = Some a | None; \
                    (None : Option(I64)) }"));
    Alcotest.test_case "partial app implicit" `Quick (fun () ->
      ignore (elab "{ type Option a = Some a | None; \
                    (Some[I64] : I64 -> Option(I64)) }"));
    Alcotest.test_case "polymorphic identity" `Quick (fun () ->
      ignore (elab "{ id = fn(x) { x }; \
                    _ : I64 = id(42); \
                    _ : Bool = id(True); () }"));
    Alcotest.test_case "match infers scrutinee from patterns" `Quick (fun () ->
      ignore (elab
        "{ type Option a = Some a | None; \
         fn(x) { match (x) { Some(y) => y, None => 0 } } }"));
    Alcotest.test_case "match with identity ctor in branch" `Quick (fun () ->
      ignore (elab
        "{ type Option a = Some a | None; \
         fn(x) { match (x) { Some(y) => Some(y), None => None } } }"));
    Alcotest.test_case "match with nested ctor, should be polymorphic" `Quick
      (fun () ->
        ignore (elab
          "{ type Option a = Some a | None; \
           fn(x) { match (x) { Some(y) => Some(Some(y)), None => None } } }"));
    Alcotest.test_case "match same nominal at different instantiations" `Quick
      (fun () ->
        ignore (elab
          "{ type Option a = Some a | None; \
           f = fn(x) { match (x) { Some(y) => y, None => 0 } }; \
           g = fn(x) { match (x) { Some(y) => y, None => True } }; \
           _ : I64 = f(Some(1)); \
           _ : Bool = g(Some(True)); () }"));
  ]

let traits =
  [
    Alcotest.test_case "trait declaration" `Quick
      (elab_ok "{ trait Eq(A) = sig { eq : A -> A -> Bool }; Eq }");
    Alcotest.test_case "impl declaration" `Quick
      (elab_ok "{ trait Eq(A) = sig { eq : A -> A -> Bool }; impl Eq(I64) = module { eq = fn(x, y) { x == y } }; 0 }");
    Alcotest.test_case "trait method dispatch" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          impl Eq(I64) = module { eq = fn(x, y) { x == y } }; Eq.eq(1, 1) }"
         "Bool");
    Alcotest.test_case "trait bound dispatch" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          impl Eq(I64) = module { eq = fn(x, y) { x == y } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(1, 1) }"
         "Bool");
    (* [open] is idempotent: the same impl arriving twice is not two impls.
       This used to report AmbiguousTraitImplementation. *)
    Alcotest.test_case "repeated open of the same impl is not ambiguous" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub impl Eq(I64) = module { eq = fn(x, y) { x == y } } }; \
          open M; open M; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(1, 1) }"
         "Bool");
    Alcotest.test_case "two different impls for one trait stay ambiguous" `Quick
      (elab_fail
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub impl Eq(I64) = module { eq = fn(x, y) { x == y } } }; \
          N = module { pub impl Eq(I64) = module { eq = fn(x, y) { x != y } } }; \
          open M; open N; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(1, 1) }");
    (* Named impls: the escape hatch that makes "impls arrive through open"
       livable. [M.eq_C] is a compile-time handle on one impl, usable in
       evidence position without opening M.
       See docs/wayfinder/topics/impl-visibility.md. *)
    Alcotest.test_case "named impl in evidence position needs no open" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub type C = R; pub impl eq_C : Eq(C) = module { eq = fn(x, y) { True } } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; \
          same[M.C, M.eq_C](M.R, M.R) }"
         "Bool");
    Alcotest.test_case "unnamed impl still needs the open" `Quick
      (elab_fail
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub type C = R; pub impl Eq(C) = module { eq = fn(x, y) { True } } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; \
          same(M.R, M.R) }");
    Alcotest.test_case "private named impl is not a member" `Quick
      (elab_fail
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub type C = R; impl eq_C : Eq(C) = module { eq = fn(x, y) { True } } }; \
          M.eq_C }");
    Alcotest.test_case "named impl resolves in its own module" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          impl eq_i64 : Eq(I64) = module { eq = fn(x, y) { x == y } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(1, 1) }"
         "Bool");
    Alcotest.test_case "missing impl rejected" `Quick
      (elab_fail
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(True, False) }");
    Alcotest.test_case "duplicate trait field rejected" `Quick
      (elab_fail "{ trait Bad(A) = sig { f : A -> A; f : A -> A }; Bad }");
    Alcotest.test_case "impl missing field rejected" `Quick
      (elab_fail "{ trait Eq(A) = sig { eq : A -> A -> Bool }; impl Eq(I64) = module { }; 0 }");
    Alcotest.test_case "struct impl for Self" `Quick
      (check_type_src
         "{ Point = struct { \
             x: I64; \
             pub impl Eq(Self) = module { eq = fn(lhs, rhs) { lhs.x == rhs.x } } \
           }; \
           Point{x = 1} == Point{x = 1} }"
         "Bool");
  ]

let effects =
  [
    Alcotest.test_case "effect instance has type Type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S; put : S -> Unit }; State(I64) }"
         U);
    Alcotest.test_case "effect family has function type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; State }"
         (pi Explicit U U));
    Alcotest.test_case "same effect family and params convert" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; \
          ((fn(x) { x } : Unit -> Unit can State(I64)) : Unit -> Unit can State(I64)) }");
    Alcotest.test_case "different effect params elaborate" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; Unit -> I64 can State(Bool) }"
         U);
    Alcotest.test_case "identical signatures remain nominal" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; \
          effect Env(S) = sig { get : Unit -> S }; \
          ((fn(x) { x } : Unit -> Unit can State(I64)) : Unit -> Unit can Env(I64)) }");
    Alcotest.test_case "public effect field through dot" `Quick
      (check_type
         "{ M = module { pub effect State(S) = sig { get : Unit -> S } }; M.State(I64) }"
         U);
    Alcotest.test_case "public effect field through open" `Quick
      (check_type
         "{ M = module { pub effect State(S) = sig { get : Unit -> S } }; open M; State(I64) }"
         U);
    Alcotest.test_case "private effect field hidden" `Quick
      (elab_fail
         "{ M = module { effect State(S) = sig { get : Unit -> S } }; M.State(I64) }");
    Alcotest.test_case "private effect usable by public member" `Quick
      (check_type
         "{ M = module { effect State(S) = sig { get : Unit -> S }; pub T = State(I64) }; M.T }"
         U);
    Alcotest.test_case "duplicate operation rejected" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S; get : Unit -> S }; State(I64) }");
    Alcotest.test_case "operation name is not bound" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; get }");
    Alcotest.test_case "imported public effect" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ]
         "{ E = import \"effects\"; E.State(I64) }" U);
    Alcotest.test_case "effectful arrow has type Type" `Quick
      (check_type
         "{ effect IO = sig { read : Unit -> I64 }; I64 -> I64 can IO }"
         U);
    Alcotest.test_case "parameterized row has type Type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; Unit -> I64 can State(I64) }"
         U);
    Alcotest.test_case "braced multi-effect row has type Type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; effect IO = sig { read : Unit -> I64 }; Unit -> I64 can {State(I64), IO} }"
         U);
    Alcotest.test_case "EffectRow has type Type" `Quick
      (check_type "EffectRow" U);
    Alcotest.test_case "open effect row has type Type" `Quick
      (check_type
         "{ effect IO = sig { read : Unit -> I64 }; [r : EffectRow] -> (Unit -> I64 can {IO | r}) }"
         U);
    Alcotest.test_case "tail-only effect row has type Type" `Quick
      (check_type
         "[r : EffectRow] -> (Unit -> I64 can {| r})"
         U);
    Alcotest.test_case "lambda checks against effectful function type" `Quick
      (elab_ok
         "{ effect IO = sig { read : Unit -> I64 }; (fn(x) { x } : I64 -> I64 can IO) }");
    Alcotest.test_case "row order ignored" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; effect IO = sig { read : Unit -> I64 }; ((fn(x) { x } : I64 -> I64 can {IO, State(I64)}) : I64 -> I64 can {State(I64), IO}) }");
    Alcotest.test_case "non-effect row entry rejected" `Quick
      (elab_fail
         "I64 -> I64 can I64");
    Alcotest.test_case "duplicate row entry rejected" `Quick
      (elab_fail
         "{ effect IO = sig { read : Unit -> I64 }; I64 -> I64 can {IO, IO} }");
    Alcotest.test_case "perform get checks in effectful lambda" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; (fn(_) { perform State.get () } : Unit -> I64 can State(I64)) }");
    Alcotest.test_case "perform put checks in effectful lambda" `Quick
      (elab_ok
         "{ effect State(S) = sig { put : S -> Unit }; (fn(_) { perform State.put(42) } : Unit -> Unit can State(I64)) }");
    Alcotest.test_case "unknown operation rejected" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; (fn(_) { perform State.missing () } : Unit -> I64 can State(I64)) }");
    Alcotest.test_case "wrong operation argument rejected" `Quick
      (elab_fail
         "{ effect State(S) = sig { put : S -> Unit }; (fn(_) { perform State.put(True) } : Unit -> Unit can State(I64)) }");
    Alcotest.test_case "pure lambda rejects perform" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; (fn(_) { perform State.get () } : Unit -> I64 can {}) }");
    Alcotest.test_case "pure lambda still checks against effectful type" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; (fn(_) { 1 } : Unit -> I64 can State(I64)) }");
    Alcotest.test_case "effectful call must be accounted for" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; f : Unit -> I64 can State(I64) = fn(_) { perform State.get () }; (fn(_) { f() } : Unit -> I64 can {}) }");
    Alcotest.test_case "effectful call propagates latent row" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; f : Unit -> I64 can State(I64) = fn(_) { perform State.get () }; (fn(_) { f() } : Unit -> I64 can State(I64)) }");
    Alcotest.test_case "unannotated higher-order wrapper threads effects" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; \
          f : Unit -> I64 can State(I64) = fn(_) { perform State.get () }; \
          wrap : (Unit -> I64) -> Unit -> I64 = fn(g) { fn(_) { g() } }; \
          (fn(_) { wrap(f)() } : Unit -> I64 can State(I64)) }");
    Alcotest.test_case "open row accepts concrete prefix effect" `Quick
      (elab_ok
         "{ effect IO = sig { read : Unit -> I64 }; \
          ((fn[r : EffectRow] { fn(_) { perform IO.read () } }) : [r : EffectRow] -> (Unit -> I64 can {IO | r})) }");
    Alcotest.test_case "tail-only row accepts empty tail" `Quick
      (elab_ok
         "((fn[r : EffectRow] { fn(_) { 1 } }) : [r : EffectRow] -> (Unit -> I64 can {| r}))");
    Alcotest.test_case "extra effect rejects against rigid explicit row" `Quick
      (elab_fail
         "{ effect IO = sig { read : Unit -> I64 }; \
           effect State(S) = sig { get : Unit -> S }; \
           ((fn[r : EffectRow] { fn(_) { _ = perform IO.read (); perform State.get () } }) : [r : EffectRow] -> (Unit -> I64 can {IO | r})) }");
    Alcotest.test_case "inferred perform lambda exposes latent effect" `Quick
      (fun () ->
        let expr = Parse_expand.parse_expr "{ effect State(S) = sig { get : Unit -> S }; fn(_) { perform State.get () } }" in
        let ctx = Elaborate.init_ctx () in
        let _core, ty, _effects = Elaborate.on_expr_effects ctx expr in
        match Nbe.force ctx.Elaborate.Ctx.metas ty with
        | VPi { effects; _ } when not (List.is_empty effects.effects) -> ()
        | _ -> Alcotest.fail "expected latent State effect");
    Alcotest.test_case "handler removes single-operation effect" `Quick
      (elab_ok
         "{ effect Exc = sig { raise : I64 -> I64 }; (fn(_) { match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n + 1 } } : Unit -> I64) }");
    Alcotest.test_case "handler continuation type checks" `Quick
      (elab_ok
         "{ effect Exc = sig { raise : I64 -> I64 }; (fn(_) { match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(n + 1) } } : Unit -> I64) }");
    Alcotest.test_case "resume argument type checked" `Quick
      (elab_fail
         "{ effect Exc = sig { raise : I64 -> I64 }; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(True) } }");
    Alcotest.test_case "lexical resume in nested lambda type checks" `Quick
      (elab_ok
         "{ effect Exc = sig { raise : I64 -> I64 }; \
          (match (perform Exc.raise(1)) { x => x, effect Exc.raise n => (fn(x) { resume(x + 1) })(n) } : I64) }");
    Alcotest.test_case "handler branch body type checked" `Quick
      (elab_fail
         "{ effect Exc = sig { raise : I64 -> I64 }; (match (perform Exc.raise(1)) { x => x, effect Exc.raise n => True } : I64) }");
    Alcotest.test_case "tuple effect branch payload checked" `Quick
      (elab_fail
         "{ effect Console = sig { log : I64 * I64 -> I64 }; \
          match (perform Console.log((1, 2))) { x => x, effect Console.log(only) => only } }");
    Alcotest.test_case "record effect branch payload checked" `Quick
      (elab_fail
         "{ Request = struct { value: I64; extra: I64; }; \
          effect Ask = sig { prompt : Request -> I64 }; \
          match (perform Ask.prompt(Request{value = 1; extra = 2})) { x => x, effect Ask.prompt Request{missing} => missing } }");
    Alcotest.test_case "multi-operation handler remains effectful when partial" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S; put : S -> Unit }; (fn(_) { match (perform State.get ()) { x => x, effect State.get () => 0 } } : Unit -> I64) }");
    Alcotest.test_case "duplicate effect branch rejected" `Quick
      (elab_fail
         "{ effect Exc = sig { raise : I64 -> I64 }; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => n, effect Exc.raise n => n } }");
    Alcotest.test_case "resume outside effect branch rejected" `Quick
      (elab_fail "resume 1");
    Alcotest.test_case "resume without argument rejected" `Quick
      (fun () ->
        match Parse_expand.parse_expr "resume" with
        | exception _ -> ()
        | _ -> Alcotest.fail "expected parse failure");
    Alcotest.test_case "full multi-operation handler removes effect" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S; put : S -> Unit }; \
          (fn(_) { match (perform State.get ()) { x => x, effect State.get () => 0, effect State.put next => resume() } } : Unit -> I64) }");
    Alcotest.test_case "parameterized handlers distinguish effect instances" `Quick
      (elab_ok
         "{ effect State(S) = sig { get : Unit -> S }; \
          StateI64 = State(I64); \
          StateBool = State(Bool); \
          (fn(_) { \
            match (if (perform StateBool.get ()) { perform StateI64.get () } else { 0 }) { \
              x => x, \
            effect StateI64.get () => resume 1, \
            effect StateBool.get () => resume True \
            } } \
           : Unit -> I64) }");
    Alcotest.test_case "parameterized handler remains effectful when instance missing" `Quick
      (elab_fail
         "{ effect State(S) = sig { get : Unit -> S }; \
          StateI64 = State(I64); \
          StateBool = State(Bool); \
          (fn(_) { \
            match (if (perform StateBool.get ()) { perform StateI64.get () } else { 0 }) { \
              x => x, \
            effect StateI64.get () => resume(1) \
            } } \
           : Unit -> I64) }");
    Alcotest.test_case "handler branch can perform handled effect" `Quick
      (elab_ok
         "{ effect Ping = sig { hit : I64 -> I64 }; \
          (match (perform Ping.hit(1)) { x => x, effect Ping.hit n => perform Ping.hit(n + 1) } : I64) }");
  ]

(* Any failure, including the enforestation errors a strict module raises when
   it uses prelude syntax it never opened. *)
let import_fail modules source () =
  with_modules modules (fun loader ->
      match elab_with_loader loader source with
      | exception _ -> ()
      | _ -> Alcotest.fail "expected failure")

let module_level_open =
  [
    Alcotest.test_case "module opens the prelude for itself" `Quick
      (check_import_type
         [ ("math", "open (import \"std\");\npub y = 1 + 1") ]
         "{ M = import \"math\"; M.y }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "module without the prelude open is strict" `Quick
      (import_fail [ ("math", "pub y = 1 + 1") ] "{ M = import \"math\"; M.y }");
    Alcotest.test_case "module open scopes over later bindings only" `Quick
      (import_fail
         [ ("base", "pub x = 5"); ("user", "pub y = x;\nopen (import \"base\")") ]
         "{ M = import \"user\"; M.y }");
    Alcotest.test_case "module open exposes an imported module's values" `Quick
      (check_import_type
         [ ("base", "pub x = 5"); ("user", "open (import \"base\");\npub y = x") ]
         "{ M = import \"user\"; M.y }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "module open does not re-export" `Quick
      (import_elab_fail
         [ ("base", "pub x = 5"); ("user", "open (import \"base\");\npub y = x") ]
         "{ M = import \"user\"; M.x }");
    Alcotest.test_case "module open exposes constructors" `Quick
      (check_import_type
         [ ("color", "pub type Color = Red | Green");
           ("user", "open (import \"color\");\npub v = Red") ]
         "{ M = import \"user\"; match (M.v) { Red => 1, Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open of a non-module is rejected" `Quick
      (import_elab_fail [ ("user", "k = 1;\nopen k;\npub y = 2") ]
         "{ M = import \"user\"; M.y }");
  ]

let imports =
  [
    Alcotest.test_case "basic import" `Quick
      (check_import_type [ ("math", "open (import \"std\"); pub x = 41; pub y = x + 1") ]
         "{ M = import \"math\"; M.y }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "private import member hidden" `Quick
      (import_elab_fail [ ("m", "open (import \"std\"); secret = 1; pub exposed = secret + 1") ]
         "{ M = import \"m\"; M.secret }");
    Alcotest.test_case "private import member usable internally" `Quick
      (check_import_type [ ("m", "open (import \"std\"); secret = 1; pub exposed = secret + 1") ]
         "{ M = import \"m\"; M.exposed }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "nested import" `Quick
      (check_import_type [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ]
         "{ W = import \"wrapper\"; W.M.x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes public value" `Quick
      (check_import_type [ ("math", "pub x = 42") ]
         "{ M = import \"math\"; open M; x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module hides private value" `Quick
      (import_elab_fail [ ("m", "secret = 1; pub exposed = 2") ]
         "{ M = import \"m\"; open M; secret }");
    Alcotest.test_case "open imported nested module" `Quick
      (check_import_type [ ("base", "pub x = 42"); ("wrapper", "pub M = import \"base\"") ]
         "{ W = import \"wrapper\"; open W; open M; x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module does not re-export" `Quick
      (import_elab_fail
         [ ("base", "pub x = 41"); ("wrapper", "open (import \"std\"); B = import \"base\"; pub y = { open B; x + 1 }") ]
         "{ W = import \"wrapper\"; W.x }");
    Alcotest.test_case "imported ADT match" `Quick
      (check_import_type
         [ ("color", "pub type Color = Red | Green | Blue; pub default = Green") ]
         "{ C = import \"color\"; match (C.default) { C.Red => 1, C.Green => 2, C.Blue => 3 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes constructors" `Quick
      (check_import_type [ ("color", "pub type Color = Red | Green") ]
         "{ C = import \"color\"; open C; match (Red) { Red => 1, Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes private constructors via match" `Quick
      (check_import_type [ ("secret", "type Hidden = Wrap I64; pub value = Wrap(1)") ]
         "{ S = import \"secret\"; open S; match (value) { Wrap(n) => n } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "repeated import" `Quick
      (check_import_type [ ("m", "pub x = 21") ]
         "{ A = import \"m\"; B = import \"m\"; A.x + B.x }" (AtomTy Atom_ty.TI64));
    (* Every repeated-import test above this one uses a closed unit, which is why
       the cache splicing a base-anchored term into a deeper context went
       unnoticed. These units are NOT closed - [import "std"] stays a free
       variable - and the second import lands at a different binder depth.
       See imported-module-elaboration-context. *)
    Alcotest.test_case "repeated import of non-closed unit" `Quick
      (check_import_type [ ("m", "open (import \"std\"); pub v = Some(1)") ]
         "{ A = import \"m\"; B = import \"m\"; match (B.v) { Some(k) => k, None => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "repeated import of non-closed unit at differing depths" `Quick
      (check_import_type [ ("m", "open (import \"std\"); pub v = Some(1)") ]
         "{ A = import \"m\"; pad = 1; B = import \"m\"; match (B.v) { Some(k) => k, None => pad } }"
         (AtomTy Atom_ty.TI64));
    (* A unit's meaning must not depend on the importer's choice of local names. *)
    Alcotest.test_case "unit cannot see importer's locals" `Quick
      (import_elab_fail [ ("u", "pub v = outer_val") ]
         "{ outer_val = 9; U = import \"u\"; U.v }");
    Alcotest.test_case "unit cannot see prelude values without its own open" `Quick
      (import_elab_fail [ ("u", "pub v = Some(1)") ]
         "{ U = import \"u\"; U.v }");
    Alcotest.test_case "unit reaches the prelude qualified without an open" `Quick
      (check_import_type [ ("u", "pub v = stdlib.Some(1)") ]
         "{ U = import \"u\"; match (U.v) { stdlib.Some(k) => k, stdlib.None => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record field access" `Quick
      (check_import_type [ ("shapes", "pub type Point = struct {x: I64; y: I64}") ]
         "{ S = import \"shapes\"; (S.Point{x = 1; y = 2}).x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern" `Quick
      (check_import_type [ ("shapes", "pub type Point = struct {x: I64; y: I64}") ]
         "{ S = import \"shapes\"; match (S.Point{x = 1; y = 2}) { S.Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern alias" `Quick
      (check_import_type [ ("shapes", "pub type Point = struct {x: I64; y: I64}") ]
         "{ S = import \"shapes\"; Alias = S; match (S.Point{x = 1; y = 2}) { Alias.Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported method uses self" `Quick
      (check_import_type
         [ ("box", "pub Box = struct { value: I64; pub method get() { self.value } }") ]
         "{ B = import \"box\"; B.Box.get(B.Box{value = 1}) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported method uses Self" `Quick
      (check_import_type
         [ ("box", "pub Box = struct { value: I64; pub method copy(other : Self) { other.value } }") ]
         "{ B = import \"box\"; B.Box.copy(B.Box{value = 1})(B.Box{value = 2}) }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported nested constructor pattern" `Quick
      (check_import_type
         [ ("nested", "pub M = module { pub type T = X(I64) | Y }") ]
         "{ N = import \"nested\"; match (N.M.X(7)) { N.M.X(n) => n, N.M.Y => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported module alias pattern" `Quick
      (check_import_type [ ("color", "pub type Color = Red | Green") ]
         "{ C = import \"color\"; Alias = C; match (C.Red) { Alias.Red => 1, Alias.Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "wrong-nominal imported constructor pattern" `Quick
      (import_elab_fail
         [ ("a", "pub type Color = Red"); ("b", "pub type Color = Red") ]
         "{ A = import \"a\"; B = import \"b\"; match (A.Red) { B.Red => 1, _ => 0 } }");
    Alcotest.test_case "imported public effect handler" `Quick
      (check_import_type [ ("effects", "pub effect Exc = sig { raise : I64 -> I64 }") ]
         "{ E = import \"effects\"; match (perform E.Exc.raise 1) { x => x, effect E.Exc.raise n => n + 1 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes effect" `Quick
      (check_import_type [ ("effects", "pub effect Exc = sig { raise : I64 -> I64 }") ]
         "{ E = import \"effects\"; open E; match (perform Exc.raise 1) { x => x, effect Exc.raise n => n + 1 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported public parameterized effect handler" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ]
         "{ E = import \"effects\"; \
           StateI64 = E.State(I64); \
           match (perform StateI64.get()) { x => x, effect StateI64.get () => 42 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported private effect hidden" `Quick
      (import_elab_fail
         [ ( "effects",
              "effect Hidden(S) = sig { get : Unit -> S }; \
               pub read : Unit -> I64 can Hidden(I64) = fn(_) { perform Hidden.get() }" ) ]
         "{ E = import \"effects\"; \
           match (E.read()) { x => x, effect E.Hidden.get () => 0 } }");
    Alcotest.test_case "imported latent effect function" `Quick
      (check_import_type
         [ ( "effects",
              "pub effect State(S) = sig { get : Unit -> S }; \
               pub read : Unit -> I64 can State(I64) = fn(_) { perform State.get() }" ) ]
         "{ E = import \"effects\"; \
           StateI64 = E.State(I64); \
           match (E.read()) { x => x, effect StateI64.get () => 7 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported parameterized handler distinguishes instances" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ]
         "{ E = import \"effects\"; \
           StateI64 = E.State(I64); \
           StateBool = E.State(Bool); \
          (fn(_) { \
             match (if (perform StateBool.get()) { perform StateI64.get() } else { 0 }) { \
              x => x, \
            effect StateI64.get () => resume(1), \
            effect StateBool.get () => resume(True) \
            } } \
           : Unit -> I64) }"
         (Pi { explicitness = Explicit; domain = AtomTy Atom_ty.TUnit; effects = empty_effect_row; codomain = AtomTy Atom_ty.TI64 }));
    Alcotest.test_case "imported private constructor hidden" `Quick
      (import_elab_fail [ ("secret", "type Hidden = Wrap I64; pub value = Wrap(1)") ]
         "{ S = import \"secret\"; match (S.value) { S.Wrap(n) => n } }");
    Alcotest.test_case "imported private member hidden through alias" `Quick
      (import_elab_fail [ ("m", "secret = 1; pub exposed = 2") ]
         "{ M = import \"m\"; Alias = M; Alias.secret }");
    Alcotest.test_case "missing import" `Quick
      (fun () ->
        let loader = Core_loader.create ~base_dir:(Filename.temp_dir "fun_core_test" "") ~builtin_syntax () in
        match elab_with_loader loader "import \"missing\"" with
        | exception Core_loader.ImportNotFound "missing" -> ()
        | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
        | _ -> Alcotest.fail "expected missing import");
    Alcotest.test_case "circular import" `Quick
      (fun () ->
        with_modules [ ("a", "pub B = import \"b\""); ("b", "pub A = import \"a\"") ]
          (fun loader ->
            match elab_with_loader loader "import \"a\"" with
            (* Reading a unit's syntax expands it, which reaches the cycle first. *)
            | exception (Core_loader.CircularImport "a" | Core_loader.CircularSyntaxVisit _) -> ()
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
    Alcotest.test_case "deref type" `Quick (check_type "{ r = ref(1); deref(r) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "assignment type" `Quick (check_type "{ r = ref(1); r <- 2 }" (AtomTy Atom_ty.TUnit));
    Alcotest.test_case "wrong assignment rejected" `Quick (elab_fail "{ r = ref(1); r <- True }");
    Alcotest.test_case "deref non-ref rejected" `Quick (elab_fail "deref(1)");
    Alcotest.test_case "assign non-ref rejected" `Quick (elab_fail "1 <- 2");
  ]

let let_rec =
  [
    Alcotest.test_case "recursive function annotated" `Quick
      (check_type
         "{ rec f : I64 -> I64 = fn(n) { if (n == 0) { 0 } else { n + f(n - 1) } }; f(5) }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "recursive identity" `Quick
      (check_type
         "{ rec f : I64 -> I64 = fn(n) { if (n == 0) { n } else { f(n - 1) } }; f }"
         (pi Explicit (AtomTy Atom_ty.TI64) (AtomTy Atom_ty.TI64)));
    Alcotest.test_case "recursive bool" `Quick
      (check_type_src
         "{ rec f : I64 -> Bool = fn(n) { if (n == 0) { True } else { f(n - 1) } }; f(3) }"
         "Bool");
  ]

let rejected source () =
  match elab source with
  | exception _ -> ()
  | _ -> Alcotest.fail ("expected rejection: " ^ source)

(* [type A = … and B = …] chains, and nested patterns through recursive
   positions, which read a placeholder's constructors by id. *)
let type_chains =
  let ab = "M = module { pub type A = MkA(B) | NoA and B = MkB(A) | NoB }" in
  [
    Alcotest.test_case "nested pattern through a self-recursive type" `Quick
      (eval_i64 "match (Cons(1, Cons(2, Nil))) { Cons(_, Cons(y, _)) => y, _ => 0 }" 2L);
    Alcotest.test_case "chain members refer to each other" `Quick
      (eval_i64 ("{ " ^ ab ^ "; match (M.MkA(M.MkB(M.NoA))) { M.MkA(M.MkB(M.NoA)) => 1, _ => 0 } }") 1L);
    Alcotest.test_case "parameterised chain" `Quick
      (eval_i64
         "{ M = module { pub type Tree(X) = Leaf(X) | Node(Forest(X)) and Forest(X) = Empty | More(Tree(X), Forest(X)) }; \
          match (M.Node(M.More(M.Leaf(7), M.Empty))) { M.Node(M.More(M.Leaf(n), _)) => n, _ => 0 } }" 7L);
    Alcotest.test_case "chain members are distinct types" `Quick
      (rejected ("{ " ^ ab ^ "; x : M.A = M.MkB(M.NoA); 0 }"));
    Alcotest.test_case "exhaustiveness sees through the chain" `Quick
      (rejected ("{ " ^ ab ^ "; match (M.MkA(M.NoB)) { M.MkA(M.MkB(_)) => 1 } }"));
    Alcotest.test_case "separate statements stay sequential" `Quick
      (rejected "{ M = module { pub type A = MkA(B) | NoA; pub type B = MkB(A) | NoB }; 0 }");
    Alcotest.test_case "duplicate chain member" `Quick
      (rejected "{ M = module { pub type A = MkA and A = NoB }; 0 }");
    Alcotest.test_case "record in a chain" `Quick
      (rejected "{ M = module { pub type A = MkA(B) and B = struct {x: A} }; 0 }");
    Alcotest.test_case "chain in a scoped do head" `Quick
      (rejected "{ type A = MkA(B) and B = MkB(A); 0 }");
  ]

(* Open choices: a bare name an open may supply resolves to the first open that
   has it, else the binder it shadows - never to a local found by spelling. *)
let open_choices =
  [
    Alcotest.test_case "a generated name is not writable" `Quick (rejected "{ x = 1; x__0 }");
    Alcotest.test_case "a name spelled like an old generated name is just a name" `Quick
      (eval_i64 "{ x = 1; x__0 = 7; x__0 }" 7L);
    Alcotest.test_case "an open's member is not shadowed by a generated name" `Quick
      (eval_i64 "{ M = module { pub x__1 = 5 }; open M; x = 1; x__1 }" 5L);
    Alcotest.test_case "an open shadows an earlier binder" `Quick
      (eval_i64 "{ M = module { pub y = 5 }; y = 1; open M; y }" 5L);
    Alcotest.test_case "a later binder shadows an open" `Quick
      (eval_i64 "{ M = module { pub y = 5 }; open M; y = 1; y }" 1L);
    Alcotest.test_case "an open without the member falls back to the binder" `Quick
      (eval_i64 "{ M = module { pub z = 5 }; y = 1; open M; y }" 1L);
    Alcotest.test_case "a type named EffectRow is an ordinary name" `Quick
      (eval_i64 "{ M = module { pub type A = MkA(EffectRow) and EffectRow = MkE(I64) }; f = fn(a : M.A) { 1 }; f(M.MkA(M.MkE(3))) }" 1L);
    (* I3: a dotted path denotes the last member of its name - for a named impl
       too, in both the type view (the elaborator) and the value view (the
       evaluator). *)
    Alcotest.test_case "a named impl path denotes the last impl of that name" `Quick
      (eval_i64
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          M = module { pub impl eq_I : Eq(Bool) = module { eq = fn(x, y) { True } }; \
                       pub impl eq_I : Eq(I64) = module { eq = fn(x, y) { False } } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; \
          if (same[I64, M.eq_I](1, 1)) { 1 } else { 2 } }"
         2L);
  ]

(* The checker evaluates under a budget: a divergent evaluation it performs is
   an error, not a hang. *)
let budget_exceeded source () =
  match elab source with
  | exception Elaborate.ElabError (Elaborate.EvaluationBudgetExceeded _) -> ()
  | _ -> Alcotest.fail ("expected an evaluation budget error: " ^ source)

(* The overrun names the fixpoint it was calling, the request that demanded
   the evaluation, and where the elaborator was. *)
let budget_message source expected () =
  match elab source with
  | exception Elaborate.ElabError (Elaborate.EvaluationBudgetExceeded { limit; call; demand; site }) ->
      Alcotest.(check string) source expected (Eval_budget.message ~limit ~call ~demand ~site)
  | _ -> Alcotest.fail ("expected an evaluation budget error: " ^ source)

let evaluation_budget =
  [
    Alcotest.test_case "a budget error names the call, the demand and the form" `Quick
      (budget_message "{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(y : loop(0)) { 1 }; 2 }"
         "evaluation exceeded the budget of 1000000 calls while type checking: calling loop, in an evaluation \
          while reading the type at <unknown>:1:57-1:64 (the budget cannot yet be raised from source)");
    Alcotest.test_case "a divergent type is a budget error" `Quick
      (budget_exceeded "{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(y : loop(0)) { 1 }; 2 }");
    Alcotest.test_case "a recursive call on an unknown variable unfolds" `Quick
      (elab_ok "{ rec double : I64 -> I64 = fn(n) { n + n }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(double(n))) { (y : F(n + n)) }; 2 }");
    Alcotest.test_case "a divergent call on an unknown variable is a budget error" `Quick
      (budget_exceeded "{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(n : I64, y : loop(n)) { 1 }; 2 }");
    Alcotest.test_case "a call passing a closure that captures an unknown variable unfolds too" `Quick
      (budget_exceeded "{ rec r : (I64 -> I64) -> Type = fn(f) { r(f) }; g = fn(n : I64, y : r(fn(z) { n })) { 1 }; 2 }");
    Alcotest.test_case "a recursive call through another on an unknown variable unfolds" `Quick
      (elab_ok "{ rec inc : I64 -> I64 = fn(n) { n + 1 }; rec twice_inc : I64 -> I64 = fn(n) { inc(inc(n)) }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(twice_inc(n))) { (y : F(n + 1 + 1)) }; 2 }");
    Alcotest.test_case "two calls of one pure fixpoint on convertible arguments convert without unfolding" `Quick
      (elab_ok "{ rec fact : I64 -> I64 can {} = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(fact(n))) { (y : F(fact(n))) }; 2 }");
    Alcotest.test_case "calls of two fixpoints with the same body unfold until the budget runs out" `Quick
      (budget_exceeded "{ rec fact : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; rec fact2 : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact2(n - 1) } }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(fact(n))) { (y : F(fact2(n))) }; 2 }");
    Alcotest.test_case "only a fixpoint known pure defers its calls" `Quick (fun () ->
        let rec fix_purity (t : Core.term) =
          match t with
          | Core.Fix (_, pure, _) -> Some pure
          | t -> List.find_map (fun (_, sub) -> fix_purity sub) (Core.subterms t)
        in
        let purity source =
          let core, _ = elab source in
          Option.get (fix_purity core)
        in
        let effect_decl = "effect State(S) = sig { get : Unit -> S }" in
        Alcotest.(check bool) "an empty closed row" true
          (purity "{ rec f : I64 -> I64 can {} = fn(n) { f(n) }; 1 }");
        Alcotest.(check bool) "an effectful row" false
          (purity ("{ " ^ effect_decl ^ "; rec f : Unit -> I64 can State(I64) = fn(u) { perform State.get () }; 1 }"));
        (* A bare arrow's row is still open today (bare-arrow-is-pure): not known pure. *)
        Alcotest.(check bool) "an open row" false
          (purity "{ rec f : I64 -> I64 = fn(n) { f(n) }; 1 }"));
    Alcotest.test_case "a closed call still evaluates" `Quick
      (elab_ok "{ rec k : I64 -> Type = fn(n) { if (n == 0) { I64 } else { k(n - 1) } }; g = fn(y : k(3)) { y + 1 }; 2 }");
    Alcotest.test_case "running a program is not budgeted" `Quick (fun () ->
        (* 2^20 - 1 calls, past the default budget, at depth 19. *)
        let source = "{ rec t : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { t(n - 1) + t(n - 1) } }; t(19) }" in
        let ctx = Elaborate.init_ctx () in
        let core, _ = Elaborate.on_expr ctx (parse_expr source) in
        match Elaborate.Ctx.run ctx core with
        | VAtom (I64 n) -> Alcotest.(check int64) source 524288L n
        | _ -> Alcotest.fail "expected an I64");
  ]

(* Path heads resolve like bare names (M12): an open shadows an earlier binder
   of the head's name, and traits and nominals are located through the entry
   the head resolves to. *)
let path_heads =
  let shadowed_m = "N = module { pub M = module { pub type T = C | D; pub type R = struct {y: I64}; pub effect E = sig { tell : I64 -> I64 } } }" in
  let outer_m = "M = module { pub type T = A | B; pub type R = struct {x: I64}; pub effect E = sig { ask : I64 -> I64 } }" in
  let under_open body = "{ " ^ outer_m ^ "; " ^ shadowed_m ^ "; open N; " ^ body ^ " }" in
  [
    Alcotest.test_case "a qualified pattern head" `Quick
      (eval_i64 (under_open "match (M.C) { M.C => 1, _ => 2 }") 1L);
    Alcotest.test_case "a record pattern's type" `Quick
      (eval_i64 (under_open "match (M.R{y = 7}) { M.R {y} => y }") 7L);
    Alcotest.test_case "perform and an effect branch" `Quick
      (eval_i64 (under_open "match (perform M.E.tell(1)) { x => x, effect M.E.tell n => n + 41 }") 42L);
    Alcotest.test_case "a type-name pattern head" `Quick
      (eval_i64 "{ M = module { pub type T = B }; f = fn(T : Type, X : Type) { { open M; match (X) { T => 1, _ => 0 } } }; f(I64, M.T) }" 1L);
    Alcotest.test_case "a qualified trait: impl, bound and method" `Quick
      (eval_i64
         "{ M = module { pub trait Same(A) = sig { same : A -> A -> I64 } }; \
          impl M.Same(I64) = module { same = fn(x, y) { 7 } }; \
          f : [A : M.Same] -> A -> I64 = fn[A : Type](x) { M.Same.same(x, x) }; f(1) }" 7L);
    Alcotest.test_case "a trait is not found by its name alone" `Quick
      (rejected "{ M = module { pub trait Same(A) = sig { same : A -> A -> I64 } }; impl Same(I64) = module { same = fn(x, y) { 7 } }; 0 }");
    Alcotest.test_case "a qualified head nested under a qualified head in a type chain" `Quick
      (eval_i64 "{ M = module { pub type A = MkA(E) and E = MkE(I64) }; match (M.MkA(M.MkE(3))) { M.MkA(M.MkE(n)) => n } }" 3L);
    Alcotest.test_case "a chain member spelled like an outer type" `Quick
      (eval_i64 "{ type E = Other(I64); M = module { pub type A = MkA(E) and E = MkE(I64) }; f = fn(a : M.A) { match (a) { M.MkA(M.MkE(n)) => n } }; f(M.MkA(M.MkE(3))) }" 3L);
    Alcotest.test_case "a chain member spelled like a prelude type" `Quick
      (eval_i64 "{ M = module { pub type A = MkA(EffectRow) and EffectRow = MkE(I64) }; f = fn(a : M.A) { match (a) { M.MkA(M.MkE(n)) => n } }; f(M.MkA(M.MkE(3))) }" 3L);
    Alcotest.test_case "a qualified head nested under a qualified head" `Quick
      (eval_i64 "{ M = module { pub type E = MkE(I64); pub type A = MkA(E) }; match (M.MkA(M.MkE(3))) { M.MkA(M.MkE(n)) => n } }" 3L);
    Alcotest.test_case "a qualified head nested under an unqualified head" `Quick
      (eval_i64 "{ M = module { pub type E = MkE(I64) }; type A = MkA(M.E); match (MkA(M.MkE(4))) { MkA(M.MkE(n)) => n } }" 4L);
    Alcotest.test_case "an unqualified head nested under a qualified head" `Quick
      (eval_i64 "{ type E = MkE(I64); M = module { pub type A = MkA(E) }; match (M.MkA(MkE(5))) { M.MkA(MkE(n)) => n } }" 5L);
    (* A quoted constructor value evaluates back to that constructor: [z]'s
       let type [G(A(1))] is quoted and evaluated again at run time. *)
    Alcotest.test_case "a quoted constructor with a payload evaluates" `Quick
      (eval_i64 "{ type T = A(I64) | B; h = fn(G : T -> Type, v : G(A(1))) { { z = v; 1 } }; h(fn(t : T) { I64 }, 5) }" 1L);
    Alcotest.test_case "a quoted constructor of a parametric type evaluates" `Quick
      (eval_i64 "{ type O(X) = N | S(X); h = fn(G : O(I64) -> Type, v : G(N)) { { z = v; 1 } }; h(fn(o : O(I64)) { I64 }, 5) }" 1L);
  ]

(* Every de Bruijn traversal reads a form's binder count from
   [Core.map_subterms]. Generalization's closedness check used to hold the
   depth constant under match branches (declining to generalize) and skip a
   perform's subterms (generalizing a lambda that captures, shifting its
   indices onto the wrong entries). *)
let binder_counts =
  [
    Alcotest.test_case "a lambda matching with a binder generalizes" `Quick
      (eval_i64 "{ f = fn(x) { match (x) { y => x } }; { _ = f(True); f(1) } }" 1L);
    Alcotest.test_case "a lambda with a multi-binding module generalizes" `Quick
      (eval_i64 "{ f = fn(x) { (module { pub a = 1; pub b = x }).b }; { _ = f(True); f(1) } }" 1L);
    Alcotest.test_case "a perform capturing an outer binding is not generalized" `Quick
      (eval_i64 "{ k = 41; effect E = sig { tell : I64 -> I64 }; g = fn(x) { (fn(u) { x })(perform E.tell(k)) }; match (g(1)) { v => v, effect E.tell n => n } }" 41L);
    Alcotest.test_case "an inserted meta in a codomain mentions its binder" `Quick
      (check_type_src "{ Endo = fn[T : Type](u : I64) { T -> T }; id_at : (B : Type) -> B -> B = fn(B : Type, x : B) { x }; f : (A : Type) -> Endo(0) = fn(A : Type) { id_at(A) }; f(I64) }" "I64 -> I64");
  ]

let () =
  Alcotest.run "elaborate"
    [
      ("binder_counts", binder_counts);
      ("evaluation_budget", evaluation_budget);
      ("constants", constants);
      ("type_chains", type_chains);
      ("open_choices", open_choices);
      ("path_heads", path_heads);
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
      ("module-level open", module_level_open);
      ("references", references);
      ("let_rec", let_rec);
    ]
