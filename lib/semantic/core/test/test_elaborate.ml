open Core_tt.Core
open Core_tt

let parse_expr s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let lexer = Sedlexing.with_tokenizer Core_lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Core_parser.expr_eof lexer

let elab source =
  let expr = parse_expr source in
  Elaborate.on_expr expr

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
          | AtomTy TI64 -> "I64"
          | AtomTy TBool -> "Bool"
          | AtomTy TUnit -> "Unit"
          | Pi _ -> "<pi>"
          | ProdTy _ -> "<prod>"
          | _ -> "<other>"))

let constants =
  [
    Alcotest.test_case "int" `Quick (check_type "42" (AtomTy TI64));
    Alcotest.test_case "unit" `Quick (check_type "()" (AtomTy TUnit));
    Alcotest.test_case "true" `Quick (check_type "true" (AtomTy TBool));
    Alcotest.test_case "false" `Quick (check_type "false" (AtomTy TBool));
  ]

let let_bindings =
  [
    Alcotest.test_case "simple let" `Quick
      (check_type "let x = 1 in x" (AtomTy TI64));
    Alcotest.test_case "let bool" `Quick
      (check_type "let b = true in b" (AtomTy TBool));
    Alcotest.test_case "let shadowing" `Quick
      (check_type "let x = true in let x = 1 in x" (AtomTy TI64));
  ]

let conditionals =
  [
    Alcotest.test_case "simple if" `Quick
      (check_type "if true then 1 else 2" (AtomTy TI64));
    Alcotest.test_case "nested if" `Quick
      (check_type "if true then if false then 1 else 2 else 3" (AtomTy TI64));
  ]

let lambdas =
  [
    Alcotest.test_case "application" `Quick
      (check_type "((fun x -> x) : I64 -> I64) 42" (AtomTy TI64));
    Alcotest.test_case "annotated identity" `Quick
      (check_type "(fun x -> x : I64 -> I64)"
         (Pi (AtomTy TI64, AtomTy TI64)));
    Alcotest.test_case "bool function" `Quick
      (check_type "(fun x -> x : Bool -> Bool)"
         (Pi (AtomTy TBool, AtomTy TBool)));
  ]

let annotations =
  [
    Alcotest.test_case "int annotation" `Quick
      (check_type "(42 : I64)" (AtomTy TI64));
    Alcotest.test_case "bool annotation" `Quick
      (check_type "(true : Bool)" (AtomTy TBool));
    Alcotest.test_case "let with annotation" `Quick
      (check_type "let x : I64 = 42 in x" (AtomTy TI64));
  ]

let tuples =
  [
    Alcotest.test_case "pair" `Quick
      (check_type "(1, true)" (ProdTy [ AtomTy TI64; AtomTy TBool ]));
    Alcotest.test_case "triple" `Quick
      (check_type "(1, 2, 3)"
         (ProdTy [ AtomTy TI64; AtomTy TI64; AtomTy TI64 ]));
  ]

let operators =
  [
    Alcotest.test_case "add" `Quick (check_type "1 + 2" (AtomTy TI64));
    Alcotest.test_case "compare" `Quick (check_type "1 == 2" (AtomTy TBool));
    Alcotest.test_case "complex" `Quick
      (check_type "let x = 1 + 2 in x * 3" (AtomTy TI64));
  ]

let elab_ok source () =
  let _core, _ty = elab source in
  ()

let elab_fail source () =
  match elab source with
  | exception Elaborate.ElabError _ -> ()
  | exception Core_tt.Unify.UnifyError _ -> ()
  | _ -> Alcotest.fail "expected elaboration error"

let dependent =
  [
    Alcotest.test_case "Type as value" `Quick
      (check_type "(I64 : Type)" U);
    Alcotest.test_case "type-passing identity" `Quick
      (elab_ok
         "((fun (T : Type) -> fun (x : T) -> x : Type -> I64 -> I64) I64 42)");
    Alcotest.test_case "type-level if" `Quick
      (elab_ok
         "let choose : Type -> Type -> Bool -> Type = fun a b c -> if c then a else b in (42 : choose I64 Bool true)");
    Alcotest.test_case "dependent return type" `Quick
      (elab_ok
         "let f : Bool -> Type = fun b -> if b then I64 else Bool in (42 : f true)");
    Alcotest.test_case "dependent return type false" `Quick
      (elab_ok
         "let f : Bool -> Type = fun b -> if b then I64 else Bool in (true : f false)");
    Alcotest.test_case "dependent mismatch" `Quick
      (elab_fail
         "let f : Bool -> Type = fun b -> if b then I64 else Bool in (true : f true)");
  ]

let meta_solving =
  [
    Alcotest.test_case "infer identity arg" `Quick
      (check_type "((fun x -> x) : I64 -> I64) 42" (AtomTy TI64));
    Alcotest.test_case "infer through let" `Quick
      (check_type "let f : I64 -> I64 = fun x -> x in f 42" (AtomTy TI64));
    Alcotest.test_case "infer lambda param from body" `Quick
      (check_type "(fun x -> x + 1 : I64 -> I64)"
         (Pi (AtomTy TI64, AtomTy TI64)));
  ]

let structs =
  [
    Alcotest.test_case "empty struct" `Quick
      (elab_ok "struct end");
    Alcotest.test_case "open struct" `Quick
      (check_type
         "let S = struct let x = 42 end in open S in x"
         (AtomTy TI64));
    Alcotest.test_case "struct with two fields" `Quick
      (elab_ok
         "let S = struct let x = 1; let y = true end in open S in if y then x else 0");
    Alcotest.test_case "nested struct open" `Quick
      (check_type
         "let Outer = struct let Inner = struct let val = 42 end end in open Outer in open Inner in val"
         (AtomTy TI64));
    Alcotest.test_case "struct field shadows" `Quick
      (check_type
         "let x = true in let S = struct let x = 42 end in open S in x"
         (AtomTy TI64));
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
      ("dependent", dependent);
      ("meta_solving", meta_solving);
      ("structs", structs);
    ]
