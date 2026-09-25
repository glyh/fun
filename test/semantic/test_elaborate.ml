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

let builtin_syntax = Macro_driver.std_syntax ()
let parse_expr source = Parse_expand.parse_expr ~open_prelude:true ~load_syntax:Macro_driver.std_load_syntax source
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

(* E11: a generative maker (its module keeps a reference alive), two tables
   made from it, then [rest]. *)
let symbol_table rest =
  "{ SymbolTable = fn(u : Unit) { module {
       table = ref(0);
       pub type Symbol = Sym(I64);
       pub intern = fn(s : I64) { table <- deref(table) + s; Sym(deref(table)) };
       pub name = fn(x : Symbol) { match (x) { Sym(n) => n } } } };
     st1 = SymbolTable(()); st2 = SymbolTable(()); " ^ rest ^ " }"

let eval_i64 source expected () =
  let ctx = Elaborate.init_ctx () in
  let core, _ = Elaborate.on_expr ctx (parse_expr source) in
  (* Run the program, as the REPL does: unbudgeted, nothing deferred. *)
  match Elaborate.Ctx.run ctx core with
  | VAtom (I64 n) -> Alcotest.(check int64) source expected n
  | v -> Alcotest.fail ("expected an I64: " ^ source ^ " got " ^ Debug.pp_value_short ctx.metas v)

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
      (check_type_of "(1, True)" "((0, False) : Tuple(2, I64, Bool))");
    Alcotest.test_case "triple" `Quick
      (check_type "(1, 2, 3)"
         (ProdTy [ AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64; AtomTy Atom_ty.TI64 ]));
    (* [Tuple(n, …)]: flat, its arity computed from [n]. *)
    Alcotest.test_case "Tuple annotation and projection" `Quick
      (check_type_src "{ p : Tuple(3, I64, Bool, String) = (1, True, \"a\"); p.2 }" "String");
    Alcotest.test_case "Tuple is flat" `Quick
      (check_type_of "(1, True, \"a\")" "((0, False, \"b\") : Tuple(3, I64, Bool, String))");
    (* An evaluation that fails while checking is an elaboration error at its form. *)
    Alcotest.test_case "Tuple with a negative count" `Quick (fun () ->
        match elab "{ T = Tuple(0 - 1); 1 }" with
        | exception Elab_error.ElabError (Elab_error.EvaluationFailed { message; site = Some _ }) ->
            Alcotest.(check string) "message" "Tuple: the number of components is negative" message
        | _ -> Alcotest.fail "expected a negative Tuple count to be an elaboration error");
    Alcotest.test_case "a panic in a type is an elaboration error" `Quick (fun () ->
        match elab "{ x : panic[Type](\"boom\") = 1; 1 }" with
        | exception Elab_error.ElabError (Elab_error.EvaluationFailed { message = "boom"; site = Some _ }) -> ()
        | _ -> Alcotest.fail "expected a panic in a type to be an elaboration error");
    Alcotest.test_case "Tuple under-applied is a type function" `Quick
      (check_type_src "Tuple(2, I64)" "Type -> Type");
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
    (* E11: a nominal's identity is its declaration and its own captures. *)
    Alcotest.test_case "a generative module's types are named by its binder" `Quick
      (eval_i64 (symbol_table "g = fn(x : st1.Symbol) { st1.name(x) }; g(st1.intern(5)) + st1.name(st1.intern(1))") 11L);
    Alcotest.test_case "two generative evaluations are different types" `Quick
      (elab_fail (symbol_table "g2 = fn(x : st2.Symbol) { 1 }; g2(st1.intern(5))"));
    Alcotest.test_case "a symbol from one table is not another's" `Quick
      (elab_fail (symbol_table "st2.name(st1.intern(5))"));
    Alcotest.test_case "an unnamed generative module's type may not escape" `Quick
      (elab_fail (symbol_table "SymbolTable(()).intern(5)"));
    Alcotest.test_case "type-case separates two generative evaluations by their stamps" `Quick
      (eval_i64 (symbol_table "f = fn(t : Type) { match (t) { st1.Symbol => 1, _ => 0 } }; f(st1.Symbol) * 10 + f(st2.Symbol)") 10L);
    Alcotest.test_case "a sealed type may not leave its binder's scope" `Quick
      (elab_fail (symbol_table "st1.intern(5)"));
    Alcotest.test_case "a sealed member's type may not reach its module's type" `Quick
      (elab_fail (symbol_table "M = module { t = SymbolTable(()); pub f = t.intern }; 1"));
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
    Alcotest.test_case "nested struct open" `Quick
      (check_type
         "{ Outer = module { pub Inner = module { pub val = 42 } }; open Outer; open Inner; val }"
         (AtomTy Atom_ty.TI64));
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
    (* A signature is a telescope: a later member reads an earlier one through
       the module it describes. *)
    Alcotest.test_case "a signature's named impl arrives through open" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; Ordered = sig { T : Type; eq_T : impl Eq(T) }; \
            same = fn(s : Ordered, a : s.T, b : s.T) { open s; Eq.eq(a, b) }; \
            M = module { pub T = I64; pub impl eq_T : Eq(I64) = module { eq = fn(x, y) { x == y } } }; \
            same(M, 1, 1) }"
         "Bool");
    Alcotest.test_case "a signature's named impl is a member" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; Ordered = sig { T : Type; eq_T : impl Eq(T) }; \
            eqv : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; \
            same = fn(s : Ordered, a : s.T, b : s.T) { eqv[s.T, s.eq_T](a, b) }; \
            M = module { pub T = I64; pub impl eq_T : Eq(I64) = module { eq = fn(x, y) { x == y } } }; \
            same(M, 1, 2) }"
         "Bool");
    Alcotest.test_case "an impl in a signature must be named" `Quick
      (fun () ->
        match elab "{ trait Eq(A) = sig { eq : A -> A -> Bool }; S = sig { T : Type; impl Eq(T) }; 1 }" with
        | exception Enforest_util.Error msg ->
            Alcotest.(check bool) "names the form" true (String.ends_with ~suffix:"an impl in a signature must be named: write name : impl Trait(Type)" msg)
        | _ -> Alcotest.fail "an anonymous impl in a sig was accepted");
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
    Alcotest.test_case "record type declaration" `Quick
      (check_type
         "{ Point = struct {x: I64; y: I64}; (Point{x = 1; y = 2}).x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "parameterized record type declaration" `Quick
      (check_type_src
         "{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; (Pair{fst = 1; snd = True}).snd }"
         "Bool");
    Alcotest.test_case "record type declaration pattern" `Quick
      (check_type
         "{ Point = struct {x: I64; y: I64}; match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "record construction field order" `Quick
      (check_type
         "{ Point = struct {x: I64; y: I64}; p = Point{y = 20; x = 10}; p.x + p.y }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "polymorphic record multiple instantiations" `Quick
      (check_type
         "{ Pair = fn[A : Type, B : Type] { struct {fst: A; snd: B} }; \
          p1 = Pair{fst = 10; snd = 20}; \
          p2 = Pair{fst = True; snd = 3}; \
          if (p2.fst) { p1.fst + p2.snd } else { 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "a record type is not a type declaration" `Quick
      (fun () ->
        match elab "{ type Point = struct {x: I64}; Point }" with
        | exception e ->
            let msg = Printexc.to_string e in
            Alcotest.(check bool) ("names the let form: " ^ msg) true (let sub = "a record type is a value" in let n = String.length sub in let rec at i = i + n <= String.length msg && (String.sub msg i n = sub || at (i + 1)) in at 0)
        | _ -> Alcotest.fail "type X = struct was accepted");
    Alcotest.test_case "a recursive record holds itself" `Quick
      (check_type
         "{ rec Numbers = struct { head : I64; tail : Option(Numbers) }; \
          l1 = Numbers{ head = 1, tail = None }; l2 = Numbers{ head = 2, tail = Some(l1) }; \
          match (l2.tail) { Some(x) => x.head, None => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "a parameterised recursive record holds itself" `Quick
      (check_type
         "{ rec L = fn(A : Type) { struct { meta : A; next : Option(L(A)) } }; \
          l1 = L(I64){ meta = 1, next = None }; l2 = L(I64){ meta = 2, next = Some(l1) }; l2.meta }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "same-shape recursive records are distinct" `Quick
      (fun () ->
        match elab
                "{ rec Numbers = struct { head : I64; tail : Option(Numbers) }; \
                 rec Scores = struct { head : I64; tail : Option(Scores) }; \
                 s = Scores{ head = 9, tail = None }; Numbers{ head = 1, tail = s.tail } }" with
        | exception Unify.UnifyError (CannotUnify msg) ->
            Alcotest.(check bool) "names the recursive occurrences" true
              (String.equal msg "recursive occurrence Numbers vs recursive occurrence Scores")
        | exception e -> Alcotest.fail ("unexpected " ^ Printexc.to_string e)
        | _ -> Alcotest.fail "same-shape recursive records unified");
    Alcotest.test_case "same-shape plain records unify" `Quick
      (check_type "{ P = struct { x : I64 }; Q = struct { x : I64 }; q : Q = P{ x = 3 }; q.x }" (AtomTy Atom_ty.TI64));
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
    Alcotest.test_case "method uses Self type" `Quick
      (check_type
         "{ Box = fn[A : Type] { struct { value: A; pub method id(other : Self) { other.value } } }; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "self outside method" `Quick
      (elab_fail "self");
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
    Alcotest.test_case "a method sees a later field" `Quick
      (check_type_src "{ C = struct { a : I64; pub method get() { self.b }; b : I64 }; C.get(C{a = 1; b = 9}) }" "I64");
    Alcotest.test_case "a field type mentioning an earlier method is a cycle" `Quick
      (fun () ->
        match elab "{ C = struct { a : I64; pub method get() { self.a }; b : get; }; 0 }" with
        | exception Elaborate.ElabError (FieldTypeMentionsMethod { field = "b"; _ }) -> ()
        | exception e -> Alcotest.fail (Printexc.to_string e)
        | _ -> Alcotest.fail "expected a field/method cycle error");
    Alcotest.test_case "an outer binding of the struct's name is still visible" `Quick
      (check_type_src "{ C = 1; C = struct { pub k = C }; C.k }" "I64");
  ]

let functors =
  [
    Alcotest.test_case "identity functor" `Quick
      (check_type
         "{ Double = fn(M : sig { x : I64 }) { module { pub doubled = M.x + M.x } }; (Double(module { pub x = 21 })).doubled }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "functor with private helper" `Quick
      (check_type
         "{ F = fn(M : sig { x : I64 }) { module { tmp = M.x; pub y = tmp + 1 } }; (F(module { pub x = 1 })).y }"
         (AtomTy Atom_ty.TI64));
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
  ]

(* ADTs as values: [enum { … }], constructors as members of the type. *)
let enums =
  let option = "Option2 = fn(A : Type) { enum { Some2(A), None2 } }; " in
  [
    (* A nested refutable sub-pattern makes the decision tree resolve columns out
       of source order; a branch still binds its variables in source order. *)
    Alcotest.test_case "a former's constructor is generic over its parameters" `Quick
      (eval_i64 ("{ " ^ option ^ "match (Option2.Some2(5)) { Option2.Some2(n) => n, Option2.None2 => 0 } }") 5L);
    Alcotest.test_case "an opened former's constructors match" `Quick
      (eval_i64 ("{ " ^ option ^ "open Option2; match (Some2(7)) { Some2(n) => n, None2 => 0 } }") 7L);
    Alcotest.test_case "Option2(I64) is one type however often it is written" `Quick
      (elab_ok ("{ " ^ option ^ "f = fn(a : Option2(I64), b : Option2(I64)) { 1 }; f(Option2(I64).None2, Option2.Some2(1)) }"));
    Alcotest.test_case "Option2(I64) is not Option2(Bool)" `Quick
      (elab_fail ("{ " ^ option ^ "x : Option2(Bool) = Option2(I64).None2; 1 }"));
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
    Alcotest.test_case "match tuple pattern" `Quick
      (check_type "match (1, True) { (x, b) => if (b) { x } else { 0 } }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "tuple or-pattern" `Quick
      (check_type
         "match (True, 1) { (True, x) | (False, x) => x }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "match nested tuple pattern" `Quick
      (check_type "match ((1, True), 2) { ((x, _), y) => x + y }" (AtomTy Atom_ty.TI64));
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
    Alcotest.test_case "named impl resolves in its own module" `Quick
      (check_type_src
         "{ trait Eq(A) = sig { eq : A -> A -> Bool }; \
          impl eq_i64 : Eq(I64) = module { eq = fn(x, y) { x == y } }; \
          same : [A : Eq] -> A -> A -> Bool = fn[A : Type](x, y) { Eq.eq(x, y) }; same(1, 1) }"
         "Bool");
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
    Alcotest.test_case "different effect params elaborate" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; Unit ->{State(Bool)} I64 }"
         U);
    Alcotest.test_case "public effect field through dot" `Quick
      (check_type
         "{ M = module { pub effect State(S) = sig { get : Unit -> S } }; M.State(I64) }"
         U);
    Alcotest.test_case "public effect field through open" `Quick
      (check_type
         "{ M = module { pub effect State(S) = sig { get : Unit -> S } }; open M; State(I64) }"
         U);
    Alcotest.test_case "private effect usable by public member" `Quick
      (check_type
         "{ M = module { effect State(S) = sig { get : Unit -> S }; pub T = State(I64) }; M.T }"
         U);
    Alcotest.test_case "imported public effect" `Quick
      (check_import_type [ ("effects", "pub effect State(S) = sig { get : Unit -> S }") ]
         "{ E = import \"effects\"; E.State(I64) }" U);
    Alcotest.test_case "effectful arrow has type Type" `Quick
      (check_type
         "{ effect IO = sig { read : Unit -> I64 }; I64 ->{IO} I64 }"
         U);
    Alcotest.test_case "parameterized row has type Type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; Unit ->{State(I64)} I64 }"
         U);
    Alcotest.test_case "braced multi-effect row has type Type" `Quick
      (check_type
         "{ effect State(S) = sig { get : Unit -> S }; effect IO = sig { read : Unit -> I64 }; Unit ->{State(I64), IO} I64 }"
         U);
    Alcotest.test_case "EffectRow has type Type" `Quick
      (check_type "EffectRow" U);
    Alcotest.test_case "open effect row has type Type" `Quick
      (check_type
         "{ effect IO = sig { read : Unit -> I64 }; [r : EffectRow] -> (Unit ->{IO | r} I64) }"
         U);
    Alcotest.test_case "tail-only effect row has type Type" `Quick
      (check_type
         "[r : EffectRow] -> (Unit ->{| r} I64)"
         U);
    (* [~>] (effect-arrow-syntax): a parameter's [~>] mints a row variable the
       function is polymorphic in; a result's collects its parameters'. *)
    (* A standalone [~>] has no parameter to collect from, so it mints its own
       variable: the value must work for every row, which a body that performs
       does not. *)
    (* A result unites what its parameters mint (multi-tail rows). *)
    Alcotest.test_case "a ~> result unites two callbacks' rows" `Quick
      (eval_i64
         "{ effect Log = sig { write : I64 -> I64 }; effect Exc = sig { raise : I64 -> I64 }; \
          f : (I64 ~> I64) -> (I64 ~> I64) ~> I64 = fn(g, h) { g(1) + h(2) }; \
          lg : I64 ->{Log} I64 = fn(n) { perform Log.write(n) }; \
          ex : I64 ->{Exc} I64 = fn(n) { perform Exc.raise(n) }; \
          a = f(fn(n : I64) { n }, fn(n : I64) { n * 10 }); \
          b = match (f(lg, fn(n : I64) { n })) { v => v, effect Log.write n => n + 100 }; \
          c = match (match (f(lg, ex)) { v => v, effect Log.write n => n }) { v => v, effect Exc.raise n => n * 1000 }; \
          a + b + c }" 123L);
    Alcotest.test_case "a row names several tails after the bar" `Quick
      (eval_i64
         "{ effect Log = sig { write : I64 -> I64 }; \
          f : [e1 : EffectRow, e2 : EffectRow] -> (I64 ->{e1} I64) -> (I64 ->{e2} I64) -> I64 ->{e1, e2} I64 \
            = fn[e1 : EffectRow, e2 : EffectRow](g, h, x) { g(x) + h(x) }; \
          lg : I64 ->{Log} I64 = fn(n) { perform Log.write(n) }; \
          match (f(lg, fn(n : I64) { n }, 5)) { v => v, effect Log.write n => n * 3 } }" 15L);
    (* Curried [~>]: only the final arrow collects, so a partial application is
       pure. *)
    Alcotest.test_case "a curried ~> collects on its final arrow" `Quick
      (eval_i64
         "{ effect Log = sig { write : I64 -> I64 }; \
          twice : (I64 ~> I64) ~> I64 ~> I64 = fn(g) { fn(x) { g(g(x)) } }; \
          lg : I64 ->{Log} I64 = fn(n) { perform Log.write(n) }; \
          pure_twice : I64 -> I64 = twice(fn(x : I64) { x + 1 }); \
          logging : I64 ->{Log} I64 = twice(lg); \
          pure_twice(3) }" 5L);
    Alcotest.test_case "~> mints inside a higher-order parameter" `Quick
      (eval_i64
         "{ twice : ((I64 ~> I64) ~> I64) ~> I64 = fn(k) { k(fn(x : I64) { x + 1 }) }; \
          twice(fn(c : I64 -> I64) { c(41) }) }" 42L);
    Alcotest.test_case "inferred perform lambda exposes latent effect" `Quick
      (fun () ->
        let expr = Parse_expand.parse_expr "{ effect State(S) = sig { get : Unit -> S }; fn(_) { perform State.get () } }" in
        let ctx = Elaborate.init_ctx () in
        let _core, ty, _effects = Elaborate.on_expr_effects ctx expr in
        match Nbe.force ctx.Elaborate.Ctx.metas ty with
        | VPi { effects; _ } when not (List.is_empty effects.effects) -> ()
        | _ -> Alcotest.fail "expected latent State effect");
    Alcotest.test_case "resume without argument rejected" `Quick
      (fun () ->
        match Parse_expand.parse_expr "resume" with
        | exception _ -> ()
        | _ -> Alcotest.fail "expected parse failure");
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
         [ ("color", "open (import \"std\");\npub type Color = Red | Green");
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
         [ ("color", "open (import \"std\");\npub type Color = Red | Green | Blue; pub default = Green") ]
         "{ C = import \"color\"; match (C.default) { C.Red => 1, C.Green => 2, C.Blue => 3 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes constructors" `Quick
      (check_import_type [ ("color", "open (import \"std\");\npub type Color = Red | Green") ]
         "{ C = import \"color\"; open C; match (Red) { Red => 1, Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "open imported module exposes private constructors via match" `Quick
      (check_import_type [ ("secret", "open (import \"std\");\ntype Hidden = Wrap I64; pub value = Wrap(1)") ]
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
      (check_import_type [ ("shapes", "pub Point = struct {x: I64; y: I64}") ]
         "{ S = import \"shapes\"; (S.Point{x = 1; y = 2}).x }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern" `Quick
      (check_import_type [ ("shapes", "pub Point = struct {x: I64; y: I64}") ]
         "{ S = import \"shapes\"; match (S.Point{x = 1; y = 2}) { S.Point {x; y} => x + y } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported record pattern alias" `Quick
      (check_import_type [ ("shapes", "pub Point = struct {x: I64; y: I64}") ]
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
         [ ("nested", "open (import \"std\");\npub M = module { pub type T = X(I64) | Y }") ]
         "{ N = import \"nested\"; match (N.M.X(7)) { N.M.X(n) => n, N.M.Y => 0 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "imported module alias pattern" `Quick
      (check_import_type [ ("color", "open (import \"std\");\npub type Color = Red | Green") ]
         "{ C = import \"color\"; Alias = C; match (C.Red) { Alias.Red => 1, Alias.Green => 2 } }"
         (AtomTy Atom_ty.TI64));
    Alcotest.test_case "wrong-nominal imported constructor pattern" `Quick
      (import_elab_fail
         [ ("a", "open (import \"std\");\npub type Color = Red"); ("b", "open (import \"std\");\npub type Color = Red") ]
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
               pub read : Unit ->{Hidden(I64)} I64 = fn(_) { perform Hidden.get() }" ) ]
         "{ E = import \"effects\"; \
           match (E.read()) { x => x, effect E.Hidden.get () => 0 } }");
    Alcotest.test_case "imported latent effect function" `Quick
      (check_import_type
         [ ( "effects",
              "pub effect State(S) = sig { get : Unit -> S }; \
               pub read : Unit ->{State(I64)} I64 = fn(_) { perform State.get() }" ) ]
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
      (import_elab_fail [ ("secret", "open (import \"std\");\ntype Hidden = Wrap I64; pub value = Wrap(1)") ]
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
    Alcotest.test_case "deref type" `Quick (check_type "{ r = ref(1); deref(r) }" (AtomTy Atom_ty.TI64));
    Alcotest.test_case "assignment type" `Quick (check_type "{ r = ref(1); r <- 2 }" (AtomTy Atom_ty.TUnit));
  ]

(* Refs in effect rows: using a reference performs [Mutate] on its hidden heap. *)
let mutates_unhandled source () =
  match elab source with
  | exception Elaborate.ElabError ((Elaborate.UnhandledEffects effs | Elaborate.EffectsInPureResult effs))
    when List.exists (fun e -> String.starts_with ~prefix:"effect Mutate" e) effs -> ()
  | exception e -> Alcotest.fail ("expected an unhandled Mutate: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected an unhandled Mutate"

let sum_to = "sum_to : I64 -> I64 = fn(n) { acc = ref(0); _ = acc <- deref(acc) + n; deref(acc) }"

let ref_effects =
  [
    Alcotest.test_case "a bare arrow mutating its parameter is rejected" `Quick
      (mutates_unhandled "{ bump : Ref(I64) -> Unit = fn(r) { r <- deref(r) + 1 }; 1 }");
    Alcotest.test_case "a local reference is pure from outside" `Quick
      (eval_i64 ("{ " ^ sum_to ^ "; sum_to(3) }") 3L);
    Alcotest.test_case "a pure function using a local reference evaluates in a type" `Quick
      (elab_ok ("{ " ^ sum_to ^ "; p : Tuple(sum_to(2), I64, Bool) = (1, True); p.1 }"));
    Alcotest.test_case "returning a local reference keeps its mutation" `Quick
      (mutates_unhandled "{ leak : I64 -> Ref(I64) = fn(n) { acc = ref(0); acc }; 1 }");
    Alcotest.test_case "a pure arrow cannot write an outer reference" `Quick
      (mutates_unhandled "{ r = ref(0); f : Unit -> Unit = fn(_) { r <- 2 }; 1 }");
    Alcotest.test_case "a block returning its reference keeps the effect" `Quick
      (mutates_unhandled "{ f : Unit -> Ref(I64) = fn(_) { { r = ref(1); r } }; 1 }");
    Alcotest.test_case "an unhandled Mutate names the reference" `Quick
      (fun () ->
        match elab "{ r = ref(0); f : Unit -> Unit = fn(_) { r <- 2 }; 1 }" with
        | exception Elaborate.ElabError ((Elaborate.UnhandledEffects effs | Elaborate.EffectsInPureResult effs)) ->
            Alcotest.(check (list string)) "names r" [ "effect Mutate(r)" ] effs
        | exception e -> Alcotest.fail (Printexc.to_string e)
        | _ -> Alcotest.fail "expected an unhandled Mutate");
    Alcotest.test_case "a module function touching its reference declares it" `Quick
      (mutates_unhandled "{ Counter = module { pub count = ref(0); pub tick : Unit -> Unit = fn(_) { count <- deref(count) + 1 } }; 1 }");
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
    Alcotest.test_case "chain members refer to each other" `Quick
      (eval_i64 ("{ " ^ ab ^ "; match (M.MkA(M.MkB(M.NoA))) { M.MkA(M.MkB(M.NoA)) => 1, _ => 0 } }") 1L);
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
  ]

(* Open choices: a bare name an open may supply resolves to the first open that
   has it, else the binder it shadows - never to a local found by spelling. *)
let open_choices =
  [
    Alcotest.test_case "a generated name is not writable" `Quick (rejected "{ x = 1; x__0 }");
    (* I3: a dotted path denotes the last member of its name - for a named impl
       too, in both the type view (the elaborator) and the value view (the
       evaluator). *)
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
    Alcotest.test_case "a divergent call on an unknown variable is a budget error" `Quick
      (budget_exceeded "{ rec loop : I64 -> Type = fn(n) { loop(n) }; g = fn(n : I64, y : loop(n)) { 1 }; 2 }");
    Alcotest.test_case "a call passing a closure that captures an unknown variable unfolds too" `Quick
      (budget_exceeded "{ rec r : (I64 -> I64) -> Type = fn(f) { r(f) }; g = fn(n : I64, y : r(fn(z) { n })) { 1 }; 2 }");
    Alcotest.test_case "a divergent mutually recursive pair in a type names a member" `Quick (fun () ->
        match elab "{ rec a : I64 -> Type = fn(n) { b(n) } and b : I64 -> Type = fn(n) { a(n) }; g = fn(n : I64, y : a(n)) { 1 }; 2 }" with
        | exception Elaborate.ElabError (Elaborate.EvaluationBudgetExceeded { call; _ }) ->
            Alcotest.(check bool) "names a or b" true (List.mem call [ "a"; "b" ])
        | _ -> Alcotest.fail "expected an evaluation budget error");
    Alcotest.test_case "calls of two fixpoints with the same body unfold until the budget runs out" `Quick
      (budget_exceeded "{ rec fact : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; rec fact2 : I64 -> I64 = fn(n) { if (n == 0) { 1 } else { n * fact2(n - 1) } }; F = fn(m : I64) { if (m == 4) { I64 } else { Bool } }; g = fn(n : I64, y : F(fact(n))) { (y : F(fact2(n))) }; 2 }");
    Alcotest.test_case "only a fixpoint known pure defers its calls" `Quick (fun () ->
        let rec fix_purity (t : Core.term) =
          match t with
          | Core.Fix { members; index } -> Some (List.nth members index).Core.fix_pure
          | t -> List.find_map (fun (_, sub) -> fix_purity sub) (Core.subterms t)
        in
        let purity source =
          let core, _ = elab source in
          Option.get (fix_purity core)
        in
        let effect_decl = "effect State(S) = sig { get : Unit -> S }" in
        Alcotest.(check bool) "an empty closed row" true
          (purity "{ rec f : I64 -> I64 = fn(n) { f(n) }; 1 }");
        Alcotest.(check bool) "an effectful row" false
          (purity ("{ " ^ effect_decl ^ "; rec f : Unit ->{State(I64)} I64 = fn(u) { perform State.get () }; 1 }"));
        (* A bare arrow is pure (E3). *)
        Alcotest.(check bool) "a bare arrow" true
          (purity "{ rec f : I64 -> I64 = fn(n) { f(n) }; 1 }");
        Alcotest.(check bool) "an inferred row" false
          (purity ("{ " ^ effect_decl ^ "; rec f : Unit ->{_} I64 = fn(u) { perform State.get () }; 1 }"));
        (* Nothing constrains a recursive [->{_}]: an error, not a default. *)
        (match elab "{ rec f : I64 ->{_} I64 = fn(n) { f(n) }; 1 }" with
         | exception Elab_error.ElabError Elab_error.UnsolvedEffectRow -> ()
         | _ -> Alcotest.fail "expected an unsolved row error"));
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
  let shadowed_m = "N = module { pub M = module { pub type T = C | D; pub R = struct {y: I64}; pub effect E = sig { tell : I64 -> I64 } } }" in
  let outer_m = "M = module { pub type T = A | B; pub R = struct {x: I64}; pub effect E = sig { ask : I64 -> I64 } }" in
  let under_open body = "{ " ^ outer_m ^ "; " ^ shadowed_m ^ "; open N; " ^ body ^ " }" in
  [
    Alcotest.test_case "a qualified pattern head" `Quick
      (eval_i64 (under_open "match (M.C) { M.C => 1, _ => 2 }") 1L);
    Alcotest.test_case "a record pattern's type" `Quick
      (eval_i64 (under_open "match (M.R{y = 7}) { M.R {y} => y }") 7L);
    Alcotest.test_case "perform and an effect branch" `Quick
      (eval_i64 (under_open "match (perform M.E.tell(1)) { x => x, effect M.E.tell n => n + 41 }") 42L);
    Alcotest.test_case "a trait is not found by its name alone" `Quick
      (rejected "{ M = module { pub trait Same(A) = sig { same : A -> A -> I64 } }; impl Same(I64) = module { same = fn(x, y) { 7 } }; 0 }");
    (* A quoted constructor value evaluates back to that constructor: [z]'s
       let type [G(A(1))] is quoted and evaluated again at run time. *)
  ]

(* Every de Bruijn traversal reads a form's binder count from
   [Core.map_subterms]. Generalization's closedness check used to hold the
   depth constant under match branches (declining to generalize) and skip a
   perform's subterms (generalizing a lambda that captures, shifting its
   indices onto the wrong entries). *)
let binder_counts =
  [
    Alcotest.test_case "an inserted meta in a codomain mentions its binder" `Quick
      (check_type_src "{ Endo = fn[T : Type](u : Type) { u -> T }; id_at : (B : Type) -> B -> B = fn(B : Type, x : B) { x }; f : (A : Type) -> Endo(A) = fn(A : Type) { id_at(A) }; f(I64) }" "I64 -> I64");
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
      ("enums", enums);
      ("match", match_tests);
      ("implicit_args", implicit_args);
      ("traits", traits);
      ("effects", effects);
      ("imports", imports);
      ("module-level open", module_level_open);
      ("references", references);
      ("ref effects", ref_effects);
      ("let_rec", let_rec);
    ]
