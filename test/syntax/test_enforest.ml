open Surface

let parse = Parse_expand.parse_expr
let parse_module = Parse_expand.parse_module

let raw_grouping () =
  match Raw_syntax.read "(a, [b], {c})" with
  | [ { datum = Group (Paren, items, _); _ } ] ->
      let has_bracket =
        List.exists
          (function { Raw_syntax.datum = Group (Bracket, _, _); _ } -> true | _ -> false)
          items
      in
      let has_brace =
        List.exists
          (function { Raw_syntax.datum = Group (Brace, _, _); _ } -> true | _ -> false)
          items
      in
      if not (has_bracket && has_brace) then Alcotest.fail "expected nested bracket and brace groups"
  | _ -> Alcotest.fail "expected one parenthesized group"

let line_and_block_comments () =
  match parse "# leading\n1 #| nested #| block |# comment |# + # inline\n2" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected comments to be skipped"

let datum_comment () =
  match parse "#_ skipped 1 + 2" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected datum comment to skip one term"

let operator_precedence () =
  match parse "1 + 2 * 3" with
  | Ap
      ( Ap (Var "+", Explicit, Atom (Atom.I64 1L)),
        Explicit,
        Ap (Ap (Var "*", Explicit, Atom (Atom.I64 2L)), Explicit, Atom (Atom.I64 3L)) ) ->
      ()
  | _ -> Alcotest.fail "expected multiplication to bind tighter than addition"

let left_associativity () =
  match parse "1 - 2 - 3" with
  | Ap
      ( Ap
          ( Var "-",
            Explicit,
            Ap (Ap (Var "-", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)) ),
        Explicit,
        Atom (Atom.I64 3L) ) ->
      ()
  | _ -> Alcotest.fail "expected left associative subtraction"

let prefix_not () =
  match parse "not true" with
  | Ap (Var "not", Explicit, Atom (Atom.Bool true)) -> ()
  | _ -> Alcotest.fail "expected prefix not application"

let ref_assignment () =
  match parse "r <- 2" with
  | RefSet (Var "r", Atom (Atom.I64 2L)) -> ()
  | _ -> Alcotest.fail "expected <- to lower to RefSet"

let field_access () =
  match parse "m.x.y" with
  | FieldAccess (FieldAccess (Var "m", "x"), "y") -> ()
  | _ -> Alcotest.fail "expected chained field access"

let curried_call () =
  match parse "f(1, 2, 3)" with
  | Ap
      ( Ap (Ap (Var "f", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 2L)),
        Explicit,
        Atom (Atom.I64 3L) ) ->
      ()
  | _ -> Alcotest.fail "expected curried call lowering"

let zero_argument_call () =
  match parse "f()" with
  | Ap (Var "f", Explicit, Atom Atom.Unit) -> ()
  | _ -> Alcotest.fail "expected zero-argument call to pass Unit"

let implicit_call () =
  match parse "id[I64](1)" with
  | Ap (Ap (Var "id", Implicit, Var "I64"), Explicit, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected bracket call to lower to implicit application"

let do_block_bindings () =
  match parse "do\n  x = 1\n  y = x + 1\n  y\nend" with
  | Let
      {
        name = "x";
        value = Atom (Atom.I64 1L);
        body =
          Let
            {
              name = "y";
              value = Ap (Ap (Var "+", Explicit, Var "x"), Explicit, Atom (Atom.I64 1L));
              body = Var "y";
              _;
            };
        _;
      } ->
      ()
  | _ -> Alcotest.fail "expected do block to lower to nested lets"

let old_syntax_fallback () =
  match parse "let x = 1 in x" with
  | Let { name = "x"; value = Atom (Atom.I64 1L); body = Var "x"; _ } -> ()
  | _ -> Alcotest.fail "expected old let syntax to keep parsing through fallback"

let malformed_do_rejected () =
  match parse "do x = end" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected malformed do block to be rejected"

let fn_arrow_body () =
  match parse "fn (x : I64) -> x" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn arrow body to lower to lambda"

let fn_block_body () =
  match parse "fn (x : I64) do x end" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn block body to lower to lambda"

let fn_implicit_then_explicit_params () =
  match parse "fn [A : Type] (x : A) -> x" with
  | Lam
      ( { name = "A"; type_ = Some (Var "Type"); explicitness = Implicit; _ },
        Lam ({ name = "x"; type_ = Some (Var "A"); explicitness = Explicit; _ }, Var "x") ) ->
      ()
  | _ -> Alcotest.fail "expected implicit fn params before explicit params"

let fn_unit_param () =
  match parse "fn () -> 1" with
  | Lam ({ name = "_"; type_ = Some (Var "Unit"); explicitness = Explicit; _ }, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected fn () to lower to Unit parameter"

let fn_rejects_late_implicit_params () =
  match parse "fn (x : I64) [A : Type] -> x" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected implicit params after explicit params to be rejected"

let if_do_else_end () =
  match parse "if true do 1 else 2 end" with
  | If { cond = Atom (Atom.Bool true); then_ = Atom (Atom.I64 1L); else_ = Atom (Atom.I64 2L) } -> ()
  | _ -> Alcotest.fail "expected if do else end shape"

let match_do_end () =
  match parse "match true do | true -> 1 | false -> 0 end" with
  | Match
      ( Atom (Atom.Bool true),
        [ ValueBranch (PatAtom (Atom.Bool true), Atom (Atom.I64 1L));
          ValueBranch (PatAtom (Atom.Bool false), Atom (Atom.I64 0L)) ] ) ->
      ()
  | _ -> Alcotest.fail "expected match do end branches"

let ref_call_deref () =
  match parse "deref(ref(1))" with
  | RefGet (RefNew (Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected deref(ref(expr)) shape"

let deref_field_is_ordinary_access () =
  match parse "r.deref" with
  | FieldAccess (Var "r", "deref") -> ()
  | _ -> Alcotest.fail "expected .deref to remain ordinary field access"

let resume_unit_call () =
  match parse "resume()" with
  | Resume (Atom Atom.Unit) -> ()
  | _ -> Alcotest.fail "expected resume() to lower to Resume Unit"

let import_shape () =
  match parse "import \"x\"" with
  | Import "x" -> ()
  | _ -> Alcotest.fail "expected import string shape"

let open_in_do_block () =
  match parse "do open M; x end" with
  | Open ("M", Var "x") -> ()
  | _ -> Alcotest.fail "expected open statement in do block"

let module_pub_value_decl () =
  match parse_module "pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected redesigned public module value"

let module_typed_value_decl () =
  match parse_module "x : I64 = 1" with
  | Module
      {
        bindings =
          [ LetBinding
              {
                name = "x";
                value = Annotated { inner = Atom (Atom.I64 1L); typ = Var "I64" };
                public = false;
              } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected typed module value to lower to annotated binding"

let do_typed_and_recursive_decls () =
  match parse "do
  x : I64 = 1
  rec fn id(n : I64) -> n
  id(x)
end" with
  | Let
      {
        name = "x";
        type_ = Some (Var "I64");
        value = Atom (Atom.I64 1L);
        body = Let { name = "id"; recursive = true; value = Lam ({ name = "n"; _ }, Var "n"); body = Ap (Var "id", Explicit, Var "x"); _ };
        _;
      } ->
      ()
  | _ -> Alcotest.fail "expected typed and recursive declarations in do block"

let module_fn_sugar () =
  match parse_module "pub fn id(x : I64) -> x" with
  | Module { bindings = [ LetBinding { name = "id"; value = Lam ({ name = "x"; type_ = Some (Var "I64"); _ }, Var "x"); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected module fn sugar to lower to lambda binding"

let named_module_decl () =
  match parse_module "pub module M do
  pub x = 1
end" with
  | Module
      {
        bindings =
          [ LetBinding
              {
                name = "M";
                public = true;
                value = Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] };
              } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected named module declaration"

let struct_do_expr () =
  match parse "struct do
  x : I64
  pub y = 2
end" with
  | Struct { con_fields = [ ("x", Var "I64") ]; bindings = [ LetBinding { name = "y"; value = Atom (Atom.I64 2L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected redesigned struct expression"

let module_old_syntax_fallback () =
  match parse_module "pub let x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected old module syntax to keep parsing through fallback"

let module_macro_not_runtime_field () =
  match parse_module "macro id = fn (stx : Stx) -> stx
pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected macro declaration to be dropped from runtime module bindings"

let suites =
  [ ( "enforest",
      [ Alcotest.test_case "raw grouping" `Quick raw_grouping;
        Alcotest.test_case "comments" `Quick line_and_block_comments;
        Alcotest.test_case "datum comment" `Quick datum_comment;
        Alcotest.test_case "operator precedence" `Quick operator_precedence;
        Alcotest.test_case "left associativity" `Quick left_associativity;
        Alcotest.test_case "prefix not" `Quick prefix_not;
        Alcotest.test_case "ref assignment" `Quick ref_assignment;
        Alcotest.test_case "field access" `Quick field_access;
        Alcotest.test_case "curried call" `Quick curried_call;
        Alcotest.test_case "zero argument call" `Quick zero_argument_call;
        Alcotest.test_case "implicit call" `Quick implicit_call;
        Alcotest.test_case "do block bindings" `Quick do_block_bindings;
        Alcotest.test_case "old syntax fallback" `Quick old_syntax_fallback;
        Alcotest.test_case "malformed do rejected" `Quick malformed_do_rejected;
        Alcotest.test_case "fn arrow body" `Quick fn_arrow_body;
        Alcotest.test_case "fn block body" `Quick fn_block_body;
        Alcotest.test_case "fn implicit then explicit params" `Quick fn_implicit_then_explicit_params;
        Alcotest.test_case "fn unit param" `Quick fn_unit_param;
        Alcotest.test_case "fn rejects late implicit params" `Quick fn_rejects_late_implicit_params;
        Alcotest.test_case "if do else end" `Quick if_do_else_end;
        Alcotest.test_case "match do end" `Quick match_do_end;
        Alcotest.test_case "ref call deref" `Quick ref_call_deref;
        Alcotest.test_case "deref field is ordinary access" `Quick deref_field_is_ordinary_access;
        Alcotest.test_case "resume unit call" `Quick resume_unit_call;
        Alcotest.test_case "import shape" `Quick import_shape;
        Alcotest.test_case "open in do block" `Quick open_in_do_block;
        Alcotest.test_case "module pub value decl" `Quick module_pub_value_decl;
        Alcotest.test_case "module typed value decl" `Quick module_typed_value_decl;
        Alcotest.test_case "do typed and recursive decls" `Quick do_typed_and_recursive_decls;
        Alcotest.test_case "module fn sugar" `Quick module_fn_sugar;
        Alcotest.test_case "named module decl" `Quick named_module_decl;
        Alcotest.test_case "struct do expr" `Quick struct_do_expr;
        Alcotest.test_case "module old syntax fallback" `Quick module_old_syntax_fallback;
        Alcotest.test_case "module macro not runtime field" `Quick module_macro_not_runtime_field;
      ] );
  ]
