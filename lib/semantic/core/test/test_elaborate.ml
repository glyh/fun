open Core_tt.Core
open Core_tt

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
          | PatternArityMismatch -> "PatternArityMismatch"))
    | Unify.UnifyError e ->
        let open Unify in
        Some (Printf.sprintf "UnifyError(%s)" (match e with
          | NonLinearSpine -> "NonLinearSpine"
          | NonVariableInSpine -> "NonVariableInSpine"
          | VarNotInSpine l -> Printf.sprintf "VarNotInSpine %d" l
          | NeutralVarNotInSpine l -> Printf.sprintf "NeutralVarNotInSpine %d" l
          | OccursCheck -> "OccursCheck"
          | CannotUnify -> "CannotUnify"
          | TupleLengthMismatch -> "TupleLengthMismatch"
          | SpineLengthMismatch -> "SpineLengthMismatch"
          | NeutralHeadMismatch -> "NeutralHeadMismatch"
          | FrameMismatch -> "FrameMismatch"
          | StructFieldMismatch -> "StructFieldMismatch"
          | NominalMismatch (n1, n2) ->
              Printf.sprintf "NominalMismatch(%s, %s)" n1 n2))
    | _ -> None)

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
         (Pi (Explicit, AtomTy TI64, AtomTy TI64)));
    Alcotest.test_case "bool function" `Quick
      (check_type "(fun x -> x : Bool -> Bool)"
         (Pi (Explicit, AtomTy TBool, AtomTy TBool)));
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
         (Pi (Explicit, AtomTy TI64, AtomTy TI64)));
  ]

let structs =
  [
    Alcotest.test_case "empty struct" `Quick
      (elab_ok "struct end");
    Alcotest.test_case "open struct" `Quick
      (check_type
         "let S = struct pub let x = 42 end in open S in x"
         (AtomTy TI64));
    Alcotest.test_case "struct with pub fields" `Quick
      (elab_ok
         "let S = struct pub let x = 1; pub let y = true end in open S in if y then x else 0");
    Alcotest.test_case "nested struct open" `Quick
      (check_type
         "let Outer = struct pub let Inner = struct pub let val = 42 end end in open Outer in open Inner in val"
         (AtomTy TI64));
    Alcotest.test_case "open only imports pub" `Quick
      (elab_fail
         "let S = struct let x = 42 end in open S in x");
    Alcotest.test_case "field access" `Quick
      (check_type
         "let S = struct pub let x = 42 end in S.x"
         (AtomTy TI64));
    Alcotest.test_case "field access boolean" `Quick
      (check_type
         "let S = struct pub let x = 1; pub let y = true end in S.y"
         (AtomTy TBool));
    Alcotest.test_case "nested field access" `Quick
      (check_type
         "let Outer = struct pub let Inner = struct pub let val = 42 end end in Outer.Inner.val"
         (AtomTy TI64));
    Alcotest.test_case "field not found" `Quick
      (elab_fail "let S = struct pub let x = 1 end in S.y");
    Alcotest.test_case "private not accessible" `Quick
      (elab_fail "let S = struct let x = 42 end in S.x");
    Alcotest.test_case "field decls" `Quick
      (elab_ok "struct x: I64; y: Bool; end");
    Alcotest.test_case "field decls with pub binding" `Quick
      (elab_ok "struct x: I64; pub let fourty_two = 42 end");
    Alcotest.test_case "pass struct through function" `Quick
      (elab_ok
         "let S = struct pub let x = 42 end in (fun s -> s.x) S");
    Alcotest.test_case "private used by pub" `Quick
      (check_type
         "let S = struct let helper = 42; pub let x = helper end in S.x"
         (AtomTy TI64));
  ]

let functors =
  [
    Alcotest.test_case "identity functor" `Quick
      (check_type
         "let Double = fun M -> struct pub let doubled = M.x + M.x end in (Double (struct pub let x = 21 end)).doubled"
         (AtomTy TI64));
    Alcotest.test_case "functor pass through" `Quick
      (elab_ok
         "let F = fun M -> struct pub let y = M.x end in let A = struct pub let x = 1 end in let B = F A in B.y");
    Alcotest.test_case "functor with private helper" `Quick
      (check_type
         "let F = fun M -> struct let tmp = M.x; pub let y = tmp + 1 end in (F (struct pub let x = 1 end)).y"
         (AtomTy TI64));
    Alcotest.test_case "compose functors" `Quick
      (elab_ok
         "let F = fun M -> struct pub let a = M.x end in let G = fun N -> struct pub let b = N.a end in (G (F (struct pub let x = 1 end))).b");
    Alcotest.test_case "higher order functor" `Quick
      (elab_ok
         "let Apply = fun F -> fun M -> F M in Apply (fun M -> struct pub let z = M.x end) (struct pub let x = 1 end)");
  ]

let tuple_proj =
  [
    Alcotest.test_case "proj first" `Quick
      (check_type "(1, true).0" (AtomTy TI64));
    Alcotest.test_case "proj second" `Quick
      (check_type "(1, true).1" (AtomTy TBool));
    Alcotest.test_case "proj triple" `Quick
      (check_type "(1, 2, 3).2" (AtomTy TI64));
    Alcotest.test_case "proj chain" `Quick
      (check_type "((1, true), 42).0.1" (AtomTy TBool));
    Alcotest.test_case "proj from let" `Quick
      (check_type "let p = (1, true) in p.0" (AtomTy TI64));
    Alcotest.test_case "proj type error" `Quick
      (elab_fail "42.0");
  ]

let adts =
  [
    Alcotest.test_case "constructor type" `Quick (fun () ->
      let _core, ty = elab "type Color = Red | Green | Blue in Red" in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "constructor should have type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "multiple constructors" `Quick (fun () ->
      (* All constructors of the same ADT have the same nominal type *)
      let _core, ty =
        elab "type Color = Red | Green | Blue in \
              let _ : Color = Red in \
              let _ : Color = Green in Color"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU");
    Alcotest.test_case "shadowing" `Quick (fun () ->
      let _core, ty =
        elab
          "type A = X | Y in \
           type B = X | Z in X"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B (most recent)"
      | _ -> Alcotest.fail "expected nominal");
    Alcotest.test_case "undefined constructor" `Quick
      (elab_fail "type Color = Red | Green in Blue");
    Alcotest.test_case "type Color accessible in body" `Quick (fun () ->
      let _core, ty = elab "type Color = Red | Green | Blue in Color" in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "Color should have type VU (Type)");
    Alcotest.test_case "pub type inside struct" `Quick (fun () ->
      let _core, ty =
        elab
          "let S = struct \
           pub type Color = Red | Green | Blue \
           end in S.Color"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VU -> ()  (* Color : Type *)
      | _ -> Alcotest.fail "S.Color should have type VU (Type)");
    Alcotest.test_case "pub type ctor via dot" `Quick (fun () ->
      let _core, ty =
        elab
          "let S = struct \
           pub type Color = Red | Green | Blue \
           end in S.Red"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "S.Red should have nominal type Color"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "private type not visible via dot" `Quick
      (elab_fail
         "let S = struct \
          type Color = Red | Green | Blue \
          end in S.Red");
    Alcotest.test_case "private type visible to later binding" `Quick (fun () ->
      let _core, ty =
        elab
          "let S = struct \
           type Color = Red | Green | Blue; \
           pub let default = Red \
           end in S.default"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Color") then
            Alcotest.fail "private type should be usable inside struct"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two structs, distinct nominal ids" `Quick (fun () ->
      let _core, _ty =
        elab
          "let S = struct pub type Color = Red | Green end in \
           let T = struct pub type Color = Blue end in \
           let _ : S.Color = S.Red in ()"
      in
      ());
    Alcotest.test_case "distinct nominal ids don't unify" `Quick
      (elab_fail
         "let S = struct pub type Color = Red | Green end in \
          let T = struct pub type Color = Blue end in \
          let _ : T.Color = S.Red in ()");
    Alcotest.test_case "parameterized ADT with payload" `Quick (fun () ->
      let _core, ty =
        elab "type Option a = Some a | None in Some {I64} 42"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "nullary pctor has nominal type" `Quick (fun () ->
      let _core, ty =
        elab "type Option a = Some a | None in None {I64}"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "None I64 should have type Option I64"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor partial application" `Quick (fun () ->
      let _core, ty =
        elab "type Option a = Some a | None in Some {I64}"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VPi _ -> ()  (* Some {I64} : I64 -> Option I64 *)
      | _ -> Alcotest.fail "Some {I64} should be a function type");
    Alcotest.test_case "multiple type params" `Quick (fun () ->
      let _core, ty =
        elab "type Result a e = Ok a | Err e in Ok {I64} {Bool} 42"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Result") then
            Alcotest.fail "Ok {I64} {Bool} 42 should have type Result I64 Bool"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor via let binding" `Quick (fun () ->
      let _core, ty =
        elab "type Option a = Some a | None in \
              let f = Some in f {I64} 42"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option nominal via let-bound ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "ctor passed through let" `Quick (fun () ->
      let _core, ty =
        elab "type Option a = Some a | None in \
              let f = Some {I64} in f 42"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "Option") then
            Alcotest.fail "expected Option via let-bound partial ctor"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "two pctor adts, distinct types" `Quick (fun () ->
      let _core, ty =
        elab "type A a = X a | Y in \
              type B a = X a | Z in X {I64} 42"
      in
      match Nbe.force (MetaContext.create ()) ty with
      | VNominal n ->
          if not (String.equal n.name "B") then
            Alcotest.fail "shadowed X should have type B"
      | _ -> Alcotest.fail "expected nominal type");
    Alcotest.test_case "distinct param instantiations don't unify" `Quick
      (elab_fail
         "type Option a = Some a | None in \
          let x : Option I64 = Some {I64} 42 in \
          let _ : Option Bool = x in ()");
  ]

let match_tests =
  [
    Alcotest.test_case "simple match nullary" `Quick
      (check_type
        "type Color = Red | Green | Blue in \
         (match Red with Red -> 1 | Green -> 2 | Blue -> 3 end : I64)"
        (AtomTy TI64));
    Alcotest.test_case "match with payload" `Quick
      (check_type
        "type Option a = Some a | None in \
         (match Some {I64} 42 with Some(x) -> x | None -> 0 end : I64)"
        (AtomTy TI64));
    Alcotest.test_case "match with wildcard" `Quick
      (check_type
        "type Color = Red | Green | Blue in \
         (match Red with Red -> 1 | _ -> 0 end : I64)"
        (AtomTy TI64));
    Alcotest.test_case "match bind variable" `Quick
      (check_type
        "type Color = Red | Green in \
         (match Red with x -> 1 end : I64)"
        (AtomTy TI64));
    Alcotest.test_case "match non ADT fails" `Quick
      (elab_fail "match 42 with _ -> 0 end");
    Alcotest.test_case "match unknown constructor" `Quick
      (elab_fail "type Color = Red in match Red with Blue -> 0 end");
    Alcotest.test_case "match payload arity mismatch" `Quick
      (elab_fail "type Option a = Some a | None in \
                  match Some I64 42 with Some -> 0 | None -> 0 end");
    Alcotest.test_case "match wrong scrutinee type" `Quick
      (elab_fail "type Color = Red | Green in match 42 with Red -> 1 end");
  ]

let implicit_args =
  [
    Alcotest.test_case "implicit inference Some 42" `Quick (fun () ->
      ignore (elab "type Option a = Some a | None in \
                    (Some 42 : Option I64)"));
    Alcotest.test_case "explicit implicit Some {I64} 42" `Quick (fun () ->
      ignore (elab "type Option a = Some a | None in \
                    (Some {I64} 42 : Option I64)"));
    Alcotest.test_case "nilary implicit None" `Quick (fun () ->
      ignore (elab "type Option a = Some a | None in \
                    (None : Option I64)"));
    Alcotest.test_case "partial app implicit" `Quick (fun () ->
      ignore (elab "type Option a = Some a | None in \
                    (Some {I64} : I64 -> Option I64)"));
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
      ("functors", functors);
      ("tuple_proj", tuple_proj);
      ("adts", adts);
      ("match", match_tests);
      ("implicit_args", implicit_args);
    ]
