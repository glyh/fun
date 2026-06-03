open Surface

let parse = Parse_expand.parse_expr
let parse_module = Parse_expand.parse_module

let parse_with_macros ?load_macros ?load_syntax source =
  let elaborate expr =
    let ctx = Elaborate.init_ctx () in
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  Parse_expand.parse_expr ?load_macros ?load_syntax ~elaborate ~eval_and_apply source

let parse_module_with_macros ?load_macros ?load_syntax source =
  let elaborate expr =
    let ctx = Elaborate.init_ctx () in
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply fn arg =
    let mc = Core.MetaContext.create () in
    Nbe.apply mc fn arg
  in
  Parse_expand.parse_module ?load_macros ?load_syntax ~elaborate ~eval_and_apply source

let with_modules modules f =
  let dir = Filename.temp_dir "fun_syntax_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = Core_loader.create ~base_dir:dir in
  f loader

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

let old_let_syntax_rejected () =
  match parse "let x = 1 in x" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected old let/in syntax to be rejected"

let malformed_do_rejected () =
  match parse "do x = end" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected malformed do block to be rejected"

let fn_arrow_body () =
  match parse "fn(x : I64) -> x" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn arrow body to lower to lambda"

let fn_block_body () =
  match parse "fn(x : I64) do x end" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn block body to lower to lambda"

let fn_implicit_then_explicit_params () =
  match parse "fn[A : Type](x : A) -> x" with
  | Lam
      ( { name = "A"; type_ = Some (Var "Type"); explicitness = Implicit; _ },
        Lam ({ name = "x"; type_ = Some (Var "A"); explicitness = Explicit; _ }, Var "x") ) ->
      ()
  | _ -> Alcotest.fail "expected implicit fn params before explicit params"

let fn_unit_param () =
  match parse "fn() -> 1" with
  | Lam ({ name = "_"; type_ = Some (Var "Unit"); explicitness = Explicit; _ }, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected fn () to lower to Unit parameter"

let fn_rejects_late_implicit_params () =
  match parse "fn(x : I64)[A : Type] -> x" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected implicit params after explicit params to be rejected"

let if_do_else_end () =
  match parse "if true do 1 else 2 end" with
  | If { cond = Atom (Atom.Bool true); then_ = Atom (Atom.I64 1L); else_ = Atom (Atom.I64 2L) } -> ()
  | _ -> Alcotest.fail "expected if do else end shape"

let match_do_end () =
  match parse "match true do true -> 1 | false -> 0 end" with
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

let module_newline_separators () =
  match parse_module "pub x = 1\npub y = x + 1" with
  | Module
      {
        bindings =
          [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true };
            LetBinding
              {
                name = "y";
                value = Ap (Ap (Var "+", Explicit, Var "x"), Explicit, Atom (Atom.I64 1L));
                public = true;
              } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected newlines to separate module items"

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

let struct_expr () =
  match parse "struct
  x : I64
  pub y = 2
end" with
  | Struct { con_fields = [ ("x", Var "I64") ]; bindings = [ LetBinding { name = "y"; value = Atom (Atom.I64 2L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected redesigned struct expression"

let struct_method_syntax () =
  match parse "struct value: I64; pub method get() -> self.value end" with
  | Struct
      {
        con_fields = [ ("value", Var "I64") ];
        bindings = [ MethodBinding { name = "get"; params = []; body = FieldAccess (Self, "value"); public = true } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected method keyword with parenthesized empty params"

let struct_fn_sugar_is_value_binding () =
  match parse "struct pub fn id(x : I64) -> x end" with
  | Struct
      {
        con_fields = [];
        bindings = [ LetBinding { name = "id"; value = Lam ({ name = "x"; type_ = Some (Var "I64"); _ }, Var "x"); public = true } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected pub fn in struct to lower to public value binding"

let module_old_let_syntax_rejected () =
  match parse_module "pub let x = 1" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected old module let syntax to be rejected"

let module_macro_not_runtime_field () =
  match parse_module "macro id(stx : Stx) -> stx
pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected macro declaration to be dropped from runtime module bindings"

let fn_with_type_application () =
  match parse "fn(x : Option(I64)) -> x" with
  | Lam ({ name = "x"; type_ = Some (Ap (Var "Option", Explicit, Var "I64")); _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected Option(I64) type application in fn param"

let fn_with_arrow_type () =
  match parse "fn(f : I64 -> Bool) -> f(1)" with
  | Lam ({ name = "f"; type_ = Some (Arrow (Explicit, None, Var "I64", None, Var "Bool")); _ },
         Ap (Var "f", Explicit, Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected arrow type in fn param"

let fn_with_product_type () =
  match parse "fn(f : I64 * Bool -> Bool) -> f(1, true)" with
  | Lam
      ( { name = "f";
          type_ = Some (Arrow (Explicit, None, ProdTy [Var "I64"; Var "Bool"], None, Var "Bool"));
          _ },
        Ap (Ap (Var "f", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.Bool true)) ) ->
      ()
  | _ -> Alcotest.fail "expected product type in fn param"

let match_constructor_payload () =
  match parse "match x do Some(y) -> y | None -> 0 end" with
  | Match (Var "x",
      [ ValueBranch (PatCon ([], "Some", [PatBind "y"]), Var "y");
        ValueBranch (PatCon ([], "None", []), Atom (Atom.I64 0L)) ]) -> ()
  | _ -> Alcotest.fail "expected constructor payload in match branch"

let match_record_pattern_shorthand () =
  match parse "match p do Point{x; y} -> x end" with
  | Match (Var "p",
      [ ValueBranch
          ( PatRecord
              { typ_path = []; typ = "Point";
                fields = [ ("x", None); ("y", None) ]; partial = false },
            Var "x" ) ]) -> ()
  | _ -> Alcotest.fail "expected record pattern shorthand in match"

let match_record_pattern_renamed_partial () =
  match parse "match p do Point{x = n; _} -> n end" with
  | Match (Var "p",
      [ ValueBranch
          ( PatRecord
              { typ_path = []; typ = "Point";
                fields = [ ("x", Some (PatBind "n")) ]; partial = true },
            Var "n" ) ]) -> ()
  | _ -> Alcotest.fail "expected record pattern renamed field with partial"

let syntax_prefix_in_do_block () =
  match parse_with_macros "do
  x = 1
  syntax prefix twice = fn(stx) -> stx
  twice x
end" with
  | Let { name = "x"; value = Atom (Atom.I64 1L); body = Var "x"; _ } -> ()
  | _ -> Alcotest.fail "expected syntax prefix to be usable in do block after binding"

let syntax_prefix_simple_do () =
  match parse_with_macros "do
  syntax prefix twice = fn(stx) -> stx
  twice 1
end" with
  | Atom (Atom.I64 1L) -> ()
  | _ -> Alcotest.fail "expected bare syntax prefix in do block"

let syntax_infix_in_do_block () =
  match parse_with_macros "do
  syntax infix ~ 15 left = fn(stx) -> stx
  1 + 2 ~ 3
end" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit,
        Prod [Atom (Atom.I64 2L); Atom (Atom.I64 3L)]) -> ()
  | _ -> Alcotest.fail "expected syntax infix with correct precedence grouping"

let syntax_prefix_in_module () =
  match parse_module_with_macros "syntax prefix twice = fn(stx) -> stx
pub test = do twice 1 end" with
  | Module
      { bindings =
          [ LetBinding
              { name = "test";
                value = Atom (Atom.I64 1L);
                public = true;
              } ] } -> ()
  | _ -> Alcotest.fail "expected syntax prefix declaration and usage in module"

let syntax_infix_associativity () =
  match parse_with_macros "do
  syntax infix ~ 15 right = fn(stx) -> stx
  1 ~ 2 ~ 3
end" with
  | Prod [Atom (Atom.I64 1L); Prod [Atom (Atom.I64 2L); Atom (Atom.I64 3L)]] -> ()
  | _ -> Alcotest.fail "expected right-associative syntax infix"

let syntax_extension_cross_module () =
  with_modules
    [ ("syntax_ops", "pub syntax prefix inc = fn(stx) -> stx\npub x = 1") ]
    (fun loader ->
      match
        parse_with_macros
          ~load_macros:(Core_loader.visit_macros loader)
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          "do
  Ops = import \"syntax_ops\"
  inc 2
end"
      with
      | Let { name = "Ops"; value = Import "syntax_ops"; body = Atom (Atom.I64 2L); _ } -> ()
      | _ -> Alcotest.fail "expected syntax extension from parsed module to be available")

let syntax_import_does_not_leak_between_parses () =
  with_modules
    [ ("syntax_ops", "pub syntax prefix inc = fn(stx) -> stx") ]
    (fun loader ->
      let _ =
        parse_with_macros
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          "import \"syntax_ops\""
      in
      match parse_with_macros "inc 1" with
      | exception _ -> ()
      | _ -> Alcotest.fail "expected imported syntax extension to stay local to its parse")

let syntax_extension_not_runtime_field () =
  match parse_module "syntax prefix hidden = fn(stx : Stx) -> stx
pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true } ] } -> ()
  | _ -> Alcotest.fail "expected syntax extension to be dropped from runtime module bindings"

let syntax_postfix_rejected_in_module () =
  match parse_module "syntax postfix foo = fn(stx) -> stx" with
  | exception Enforest.Unsupported "unsupported syntax declaration shape" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected unsupported module syntax declaration"

let syntax_postfix_rejected_in_do_block () =
  match parse "do syntax postfix foo = fn(stx) -> stx; 0 end" with
  | exception Enforest.Error "unsupported syntax declaration shape in do block" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected unsupported do-block syntax declaration"

let syntax_infix_bad_assoc_rejected () =
  match parse_module "syntax infix ~ 15 middle = fn(stx) -> stx" with
  | exception Enforest.Error "syntax infix associativity must be 'left' or 'right'" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected invalid syntax infix associativity to be rejected"

let syntax_extension_circular_visit () =
  with_modules
    [ ("a", "pub syntax prefix aop = fn(stx) -> do import \"b\"; stx end");
      ("b", "pub syntax prefix bop = fn(stx) -> do import \"a\"; stx end") ]
    (fun loader ->
      match
        parse_with_macros
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          "import \"a\""
      with
      | exception Core_loader.CircularSyntaxVisit "a" -> ()
      | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
      | _ -> Alcotest.fail "expected circular syntax visit")

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
        Alcotest.test_case "old let syntax rejected" `Quick old_let_syntax_rejected;
        Alcotest.test_case "malformed do rejected" `Quick malformed_do_rejected;
        Alcotest.test_case "fn arrow body" `Quick fn_arrow_body;
        Alcotest.test_case "fn block body" `Quick fn_block_body;
        Alcotest.test_case "fn implicit then explicit params" `Quick fn_implicit_then_explicit_params;
        Alcotest.test_case "fn unit param" `Quick fn_unit_param;
        Alcotest.test_case "fn rejects late implicit params" `Quick fn_rejects_late_implicit_params;
        Alcotest.test_case "if do else end" `Quick if_do_else_end;
        Alcotest.test_case "match do end" `Quick match_do_end;
        Alcotest.test_case "ref call deref" `Quick ref_call_deref;
        Alcotest.test_case "module newline separators" `Quick module_newline_separators;
        Alcotest.test_case "resume unit call" `Quick resume_unit_call;
        Alcotest.test_case "import shape" `Quick import_shape;
        Alcotest.test_case "open in do block" `Quick open_in_do_block;
        Alcotest.test_case "module pub value decl" `Quick module_pub_value_decl;
        Alcotest.test_case "module typed value decl" `Quick module_typed_value_decl;
        Alcotest.test_case "do typed and recursive decls" `Quick do_typed_and_recursive_decls;
        Alcotest.test_case "module fn sugar" `Quick module_fn_sugar;
        Alcotest.test_case "named module decl" `Quick named_module_decl;
        Alcotest.test_case "struct expr" `Quick struct_expr;
        Alcotest.test_case "struct method syntax" `Quick struct_method_syntax;
        Alcotest.test_case "struct fn sugar is value binding" `Quick struct_fn_sugar_is_value_binding;
        Alcotest.test_case "module old let syntax rejected" `Quick module_old_let_syntax_rejected;
        Alcotest.test_case "module macro not runtime field" `Quick module_macro_not_runtime_field;
        Alcotest.test_case "fn with type application" `Quick fn_with_type_application;
        Alcotest.test_case "fn with arrow type" `Quick fn_with_arrow_type;
        Alcotest.test_case "fn with product type" `Quick fn_with_product_type;
        Alcotest.test_case "match constructor payload" `Quick match_constructor_payload;
        Alcotest.test_case "match record pattern shorthand" `Quick match_record_pattern_shorthand;
        Alcotest.test_case "match record pattern renamed partial" `Quick match_record_pattern_renamed_partial;
        Alcotest.test_case "syntax prefix in do block" `Quick syntax_prefix_in_do_block;
        Alcotest.test_case "syntax prefix simple do" `Quick syntax_prefix_simple_do;
        Alcotest.test_case "syntax infix in do block" `Quick syntax_infix_in_do_block;
        Alcotest.test_case "syntax prefix in module" `Quick syntax_prefix_in_module;
        Alcotest.test_case "syntax infix associativity" `Quick syntax_infix_associativity;
        Alcotest.test_case "syntax extension cross module" `Quick syntax_extension_cross_module;
        Alcotest.test_case "syntax import does not leak between parses" `Quick syntax_import_does_not_leak_between_parses;
        Alcotest.test_case "syntax extension not runtime field" `Quick syntax_extension_not_runtime_field;
        Alcotest.test_case "syntax postfix rejected in module" `Quick syntax_postfix_rejected_in_module;
        Alcotest.test_case "syntax postfix rejected in do block" `Quick syntax_postfix_rejected_in_do_block;
        Alcotest.test_case "syntax infix bad assoc rejected" `Quick syntax_infix_bad_assoc_rejected;
        Alcotest.test_case "syntax extension circular visit" `Quick syntax_extension_circular_visit;
      ] );
  ]
