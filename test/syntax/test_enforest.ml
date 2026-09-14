open Shape

(* Operators are prelude [pub infix]/[pub prefix] declarations under the strict
   phase rule, so raw-parse tests open the prelude ([~open_prelude:true], the
   exports resolved via [std_load_syntax]) to exercise [+]/[==]/[not]/…. That
   wraps the body in [Open (Import "std", body)]; [unwrap_std] peels it back off
   so the structural assertions match the bare parse as before. *)
let builtin_syntax = Lazy.force Elab_prelude.stdlib_syntax_exports
let unwrap_std (e : Shape.t) : Shape.t =
  match e with Open (Import "std", body, _) -> body | other -> other
let parse source = unwrap_std (Parse_written.parse_expr ~open_prelude:true ~load_syntax:Elab_prelude.std_load_syntax source)
let parse_module source = Parse_written.parse_module ~load_syntax:Elab_prelude.std_load_syntax source

let string_contains text needle =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec go i =
    i + needle_len <= text_len
    && (String.equal (String.sub text i needle_len) needle || go (i + 1))
  in
  String.equal needle "" || go 0

let parse_with_macros ?load_macros ?(load_syntax = Elab_prelude.std_load_syntax) source =
  let ctx = Elaborate.init_ctx () in
  let syntax_nominals = Elaborate.syntax_nominals ctx in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply = Nbe.apply_macro in
  unwrap_std (Parse_written.parse_expr ?load_macros ~open_prelude:true ~load_syntax ~elaborate ~eval_and_apply ~syntax_nominals source)

let parse_module_with_macros ?load_macros ?(load_syntax = Elab_prelude.std_load_syntax) source =
  let ctx = Elaborate.init_ctx () in
  let syntax_nominals = Elaborate.syntax_nominals ctx in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ctx expr in
    Elaborate.Ctx.eval ctx core
  in
  let eval_and_apply = Nbe.apply_macro in
  Parse_written.parse_module ?load_macros ~load_syntax ~elaborate ~eval_and_apply ~syntax_nominals source

let with_modules modules f =
  let dir = Filename.temp_dir "fun_syntax_test" "" in
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir (name ^ ".fun") in
      Out_channel.with_open_text path (fun oc -> output_string oc source))
    modules;
  let loader = Core_loader.create ~base_dir:dir ~builtin_syntax () in
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

let dollar_token_is_separate () =
  match Raw_syntax.read "$x" with
  | [ { datum = Token { kind = Operator "$"; _ }; _ }; { datum = Token { kind = Ident "x"; _ }; _ } ] -> ()
  | _ -> Alcotest.fail "expected $ to be tokenized separately from the identifier"

let strict_phase_rule_operators_need_std () =
  (* Strict phase rule: with no [open (import "std")] and no prelude seed, the
     arithmetic operators are unknown, so [1 + 2] does not parse as an operator
     application (it fails on the dangling [+]). Contrast [operator_precedence],
     which parses the same shape after opening the prelude. *)
  match Parse_written.parse_expr "1 + 2" with
  | exception _ -> ()
  | Ap (Ap (Var "+", _, _), _, _) ->
      Alcotest.fail "expected + to be unknown without opening std"
  | _ -> ()

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
  match parse "not True" with
  | Ap (Var "not", Explicit, Var "True") -> ()
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
  match parse "{\n  x = 1;\n  y = x + 1;\n  y\n}" with
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
  match parse "{ x = }" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected malformed do block to be rejected"

let fn_arrow_body () =
  match parse "fn(x : I64) { x }" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn arrow body to lower to lambda"

let fn_block_body () =
  match parse "fn(x : I64) { x }" with
  | Lam ({ name = "x"; type_ = Some (Var "I64"); explicitness = Explicit; _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected fn block body to lower to lambda"

let fn_implicit_then_explicit_params () =
  match parse "fn[A : Type](x : A) { x }" with
  | Lam
      ( { name = "A"; type_ = Some (Var "Type"); explicitness = Implicit; _ },
        Lam ({ name = "x"; type_ = Some (Var "A"); explicitness = Explicit; _ }, Var "x") ) ->
      ()
  | _ -> Alcotest.fail "expected implicit fn params before explicit params"

let fn_unit_param () =
  match parse "fn() { 1 }" with
  | Lam ({ name = "_"; type_ = Some (Var "Unit"); explicitness = Explicit; _ }, Atom (Atom.I64 1L)) -> ()
  | _ -> Alcotest.fail "expected fn () to lower to Unit parameter"

let fn_rejects_late_implicit_params () =
  match parse "fn(x : I64)[A : Type] { x }" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected implicit params after explicit params to be rejected"

let if_do_else_end () =
  match parse "if (True) { 1 } else { 2 }" with
  | Match
      ( Var "True",
        [ ValueBranch (PatCon ([], "True", []), Atom (Atom.I64 1L));
          ValueBranch (PatCon ([], "False", []), Atom (Atom.I64 2L)) ] ) ->
      ()
  | _ -> Alcotest.fail "expected if { else } shape"

let match_do_end () =
  match parse "match (True) { True => 1 | False => 0 }" with
  | Match
      ( Var "True",
        [ ValueBranch (PatCon ([], "True", []), Atom (Atom.I64 1L));
          ValueBranch (PatCon ([], "False", []), Atom (Atom.I64 0L)) ] ) ->
      ()
  | _ -> Alcotest.fail "expected match { } branches"

let ref_call_deref () =
  match parse "deref(ref(1))" with
  | RefGet (RefNew (Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected deref(ref(expr)) shape"

let module_newline_separators () =
  match parse_module "open (import \"std\");\npub x = 1;\npub y = x + 1" with
  | Module
      {
        bindings =
          [ OpenBinding (_, _);
            LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true ; _};
            LetBinding
              {
                name = "y";
                value = Ap (Ap (Var "+", Explicit, Var "x"), Explicit, Atom (Atom.I64 1L));
                public = true;
                recursive = false;
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
  match parse "{ open M; x }" with
  | Open (Var "M", Var "x", _) -> ()
  | _ -> Alcotest.fail "expected open statement in do block"

let module_level_open_shape () =
  match parse_module "open M;\npub x = 1" with
  | Module { bindings = [ OpenBinding (Var "M", _); LetBinding { name = "x"; _ } ] } -> ()
  | _ -> Alcotest.fail "expected module-level open binding"

let module_level_open_import_shape () =
  match parse_module "open (import \"std\");\npub x = 1" with
  | Module { bindings = [ OpenBinding (Import "std", _); LetBinding { name = "x"; _ } ] } -> ()
  | _ -> Alcotest.fail "expected module-level open of import"

let struct_level_open_shape () =
  match parse_module "S = struct { open M; pub y = 1 }" with
  | Module
      { bindings =
          [ LetBinding
              { name = "S";
                value = Struct { bindings = [ OpenBinding (Var "M", _); LetBinding { name = "y"; _ } ]; _ };
                _ } ] } -> ()
  | _ -> Alcotest.fail "expected struct-level open binding"

let module_level_pub_open_rejected () =
  match parse_module "pub open M" with
  | exception Enforest_util.Error _ -> ()
  | _ -> Alcotest.fail "expected pub open to be rejected"

(* Modules are strict about the prelude: without an [open (import "std")] the
   prelude operators are not in the parse env, so [+] does not enforest. *)
let module_without_open_has_no_prelude_operators () =
  match parse_module "pub y = 1 + 1" with
  | exception Enforest_util.Unsupported _ -> ()
  | exception Enforest_util.Error _ -> ()
  | _ -> Alcotest.fail "expected prelude operator to be unavailable"

let module_open_scopes_later_statements_only () =
  (* The harvest happens where the open is written, so a use *before* it does
     not see the operator while a use after it does. *)
  (match parse_module "pub a = 1 + 1;\nopen (import \"std\")" with
   | exception Enforest_util.Unsupported _ -> ()
   | exception Enforest_util.Error _ -> ()
   | _ -> Alcotest.fail "expected operator before the open to be unavailable");
  match parse_module "open (import \"std\");\npub a = 1 + 1" with
  | Module { bindings = [ OpenBinding (_, _); LetBinding { name = "a"; _ } ] } -> ()
  | _ -> Alcotest.fail "expected operator after the open to enforest"

let module_pub_value_decl () =
  match parse_module "pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true ; _} ] } -> ()
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
                recursive = false;
              } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected typed module value to lower to annotated binding"

let do_typed_and_recursive_decls () =
  match parse "{
  x : I64 = 1;
  rec fn id(n : I64) { n };
  id(x)
}" with
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
  match parse_module "pub fn id(x : I64) { x }" with
  | Module { bindings = [ LetBinding { name = "id"; value = Lam ({ name = "x"; type_ = Some (Var "I64"); _ }, Var "x"); public = true; _ } ] } -> ()
  | _ -> Alcotest.fail "expected module fn sugar to lower to lambda binding"

let named_module_decl () =
  match parse_module "pub M = module {
  pub x = 1
}" with
  | Module
      {
        bindings =
          [ LetBinding
              {
                name = "M";
                public = true;
                recursive = false;
                value = Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true ; _} ] };
              } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected named module declaration"

let struct_expr () =
  match parse "struct {
  x : I64;
  pub y = 2
}" with
  | Struct { con_fields = [ ("x", Var "I64") ]; bindings = [ LetBinding { name = "y"; value = Atom (Atom.I64 2L); public = true ; _} ] } -> ()
  | _ -> Alcotest.fail "expected redesigned struct expression"

let struct_method_syntax () =
  match parse "struct { value: I64; pub method get() { self.value } }" with
  | Struct
      {
        con_fields = [ ("value", Var "I64") ];
        bindings = [ MethodBinding { name = "get"; params = []; body = FieldAccess (Self, "value"); public = true } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected method keyword with parenthesized empty params"

let struct_fn_sugar_is_value_binding () =
  match parse "struct { pub fn id(x : I64) { x } }" with
  | Struct
      {
        con_fields = [];
        bindings = [ LetBinding { name = "id"; value = Lam ({ name = "x"; type_ = Some (Var "I64"); _ }, Var "x"); public = true; _ } ];
      } ->
      ()
  | _ -> Alcotest.fail "expected pub fn in struct to lower to public value binding"

let module_old_let_syntax_rejected () =
  match parse_module "pub let x = 1" with
  | exception _ -> ()
  | _ -> Alcotest.fail "expected old module let syntax to be rejected"

let module_macro_not_runtime_field () =
  match parse_module "macro id(stx : Stx) { stx };
pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true ; _} ] } -> ()
  | _ -> Alcotest.fail "expected macro declaration to be dropped from runtime module bindings"

let fn_with_type_application () =
  match parse "fn(x : Option(I64)) { x }" with
  | Lam ({ name = "x"; type_ = Some (Ap (Var "Option", Explicit, Var "I64")); _ }, Var "x") -> ()
  | _ -> Alcotest.fail "expected Option(I64) type application in fn param"

let fn_with_arrow_type () =
  match parse "fn(f : I64 -> Bool) { f(1) }" with
  | Lam ({ name = "f"; type_ = Some (Arrow (Explicit, None, Var "I64", None, Var "Bool")); _ },
         Ap (Var "f", Explicit, Atom (Atom.I64 1L))) -> ()
  | _ -> Alcotest.fail "expected arrow type in fn param"

let fn_with_product_type () =
  match parse "fn(f : I64 * Bool -> Bool) { f(1, True) }" with
  | Lam
      ( { name = "f";
          type_ = Some (Arrow (Explicit, None, ProdTy [Var "I64"; Var "Bool"], None, Var "Bool"));
          _ },
        Ap (Ap (Var "f", Explicit, Atom (Atom.I64 1L)), Explicit, Var "True") ) ->
      ()
  | _ -> Alcotest.fail "expected product type in fn param"

let match_constructor_payload () =
  match parse "match (x) { Some(y) => y | None => 0 }" with
  | Match (Var "x",
      [ ValueBranch (PatCon ([], "Some", [PatBind "y"]), Var "y");
        ValueBranch (PatCon ([], "None", []), Atom (Atom.I64 0L)) ]) -> ()
  | _ -> Alcotest.fail "expected constructor payload in match branch"

let match_record_pattern_shorthand () =
  match parse "match (p) { Point{x; y} => x }" with
  | Match (Var "p",
      [ ValueBranch
          ( PatRecord
              { typ_path = []; typ = "Point";
                fields = [ ("x", Some (PatBind "x")); ("y", Some (PatBind "y")) ]; partial = false },
            Var "x" ) ]) -> ()
  | _ -> Alcotest.fail "expected record pattern shorthand in match"

let match_record_pattern_renamed_partial () =
  match parse "match (p) { Point{x = n; _} => n }" with
  | Match (Var "p",
      [ ValueBranch
          ( PatRecord
              { typ_path = []; typ = "Point";
                fields = [ ("x", Some (PatBind "n")) ]; partial = true },
            Var "n" ) ]) -> ()
  | _ -> Alcotest.fail "expected record pattern renamed field with partial"

let operator_prefix_in_do_block () =
  match parse_with_macros "{
  x = 1;
  syntax twice { | twice $x => $x };
  twice x
}" with
  | Let { name = "x"; value = Atom (Atom.I64 1L); body = Var "x"; _ } -> ()
  | _ -> Alcotest.fail "expected operator prefix to be usable in do block after binding"

let operator_prefix_simple_do () =
  match parse_with_macros "{
  syntax twice { | twice $x => 1 };
  twice 1
}" with
  | Atom (Atom.I64 1L) -> ()
  | _ -> Alcotest.fail "expected bare operator prefix in do block"

let operator_infix_in_do_block () =
  match parse_with_macros "{
  infix (~) 15 Left (stx) { Syntax.i64(9) };
  1 + 2 ~ 3
}" with
  | Ap (Ap (Var "+", Explicit, Atom (Atom.I64 1L)), Explicit, Atom (Atom.I64 9L)) -> ()
  | _ -> Alcotest.fail "expected operator infix with correct precedence grouping"

let operator_prefix_in_module () =
  match parse_module_with_macros "syntax twice { | twice $x => 1 };
pub test = { twice 1 }" with
  | Module
      { bindings =
          [ LetBinding
              { name = "test";
                value = Atom (Atom.I64 1L);
                public = true;
                recursive = false;
              } ] } -> ()
  | _ -> Alcotest.fail "expected operator prefix declaration and usage in module"

let operator_rhs_can_use_earlier_macro () =
  match parse_with_macros "{
  macro one_body(_) { Syntax.i64(1) };
  syntax choose { | choose $x => one_body(0) };
  choose 0
}" with
  | Atom (Atom.I64 1L) -> ()
  | _ -> Alcotest.fail "expected operator RHS to expand through the macro path"



let operator_declaration_is_sequential () =
  match parse_with_macros "{ before = late 0; syntax late { | late $x => 1 }; late 0 }" with
  | exception Enforest.Unsupported _ -> ()
  | exception Enforest.Error _ -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected operator declaration to affect only later statements"

let duplicate_operator_later_wins () =
  match parse_with_macros "{
  syntax choose { | choose $x => 1 };
  first = choose 0;
  syntax choose { | choose $x => 2 };
  choose 0
}" with
  | Let { name = "first"; value = Atom (Atom.I64 1L); body = Atom (Atom.I64 2L); _ } -> ()
  | _ -> Alcotest.fail "expected later operator declaration to shadow earlier declaration"

let syntax_extension_cross_module () =
  with_modules
    [ ("syntax_ops", "pub syntax inc { | inc $x => 2 };\npub x = 1") ]
    (fun loader ->
      match
        parse_with_macros
          ~load_macros:(Macro_driver.visit_macros loader)
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          "{
  Ops = import \"syntax_ops\";
  inc 2
}"
      with
      | Let { name = "Ops"; value = Import "syntax_ops"; body = Atom (Atom.I64 2L); _ } -> ()
      | _ -> Alcotest.fail "expected syntax extension from parsed module to be available")

let syntax_import_does_not_leak_between_parses () =
  with_modules
    [ ("syntax_ops", "pub syntax inc { | inc $x => 1 }") ]
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
  match parse_module "syntax hidden { | hidden $x => $x };
pub x = 1" with
  | Module { bindings = [ LetBinding { name = "x"; value = Atom (Atom.I64 1L); public = true ; _} ] } -> ()
  | _ -> Alcotest.fail "expected syntax extension to be dropped from runtime module bindings"

let pub_syntax_rejected_in_struct () =
  match parse "struct { pub syntax hidden { | hidden $x => $x } }" with
  | exception Enforest.Error msg when string_contains msg "pub syntax is not supported inside structs" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected pub syntax in a struct expression to be rejected"

let syntax_exports_include_operator_metadata () =
  match Enforest.parse_public_syntax_exports "pub infix (~) 15 Right (stx) { stx }" with
  | [ { symbol = "~"; fixity = Binding.Infix; precedence = 15; associativity = Binding.Right;
        syntax_class = Syntax_class.Expr; expansion = Binding.MacroOp; _ } ] ->
      ()
  | _ -> Alcotest.fail "expected public syntax export to include operator metadata"

let duplicate_public_syntax_exports_rejected () =
  match
    Enforest.parse_public_syntax_exports "pub syntax dup { | dup $x => $x };
pub syntax dup { | dup $x => $x }"
  with
  | exception Enforest.Error msg when string_contains msg "ambiguous syntax extension candidates" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected duplicate public syntax exports to be rejected"

let syntax_branch_head_mismatch_rejected () =
  match parse_module "syntax good { | bad $x => $x }" with
  | exception Enforest.Error msg when string_contains msg "must start with declared head" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected mismatched syntax branch head to be rejected"

let syntax_unbound_replacement_hole_rejected () =
  match parse_module "syntax good { | good $x => $y }" with
  | exception Enforest.Error msg when string_contains msg "unbound syntax template hole" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected unbound replacement hole to be rejected"

(* A hole's kind is its reflection type (M10); [Id] names a binder or refers,
   the splice position decides which. *)
let syntax_id_hole_binds_and_refers () =
  match parse_with_macros "{ syntax twice_bound { | twice_bound $(name : Id) => { $name = 1; $name } }; twice_bound x }" with
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> ()

let syntax_old_hole_kind_spelling_rejected () =
  match parse_with_macros "{ syntax bad { | bad $(name: ident) => $name }; bad x }" with
  | exception Enforest.Error msg when string_contains msg "written as types" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected a lowercase hole kind to be rejected"

let syntax_pattern_hole_expression_position_rejected () =
  match parse_with_macros "{ syntax bad { | bad $(p : Pattern) => $p }; bad x }" with
  | exception Enforest.Error msg when string_contains msg "pattern hole used in expression position" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected a pattern hole in expression position to be rejected"

let syntax_multi_rejected () =
  match parse_module "syntax bad : Decl { | bad => multi { x = 1 } }" with
  | exception Enforest.Error msg when string_contains msg "multi { … } was removed" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected multi to be rejected"

let syntax_postfix_rejected_in_module () =
  match parse_module "operator postfix foo(stx) -> stx" with
  | exception (Enforest.Unsupported msg) ->
      if String.starts_with ~prefix:"unsupported module item" msg then ()
      else Alcotest.fail ("unexpected message: " ^ msg)
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
   | _ -> Alcotest.fail "expected unsupported module syntax declaration"

let syntax_postfix_rejected_in_do_block () =
  match parse "{ operator postfix foo(stx) -> stx; 0 }" with
  | exception (Enforest.Error _ | Enforest.Unsupported _) -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected unsupported do-block syntax declaration"

let old_syntax_declaration_rejected () =
  match parse_module "operator prefix foo(stx) -> stx" with
  | exception (Enforest.Unsupported msg) ->
      if String.starts_with ~prefix:"unsupported module item" msg then ()
      else Alcotest.fail ("unexpected message: " ^ msg)
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected old syntax declaration spelling to be rejected"

let operator_infix_bad_assoc_rejected () =
  match parse_module "infix (~) 15 middle (stx) { stx }" with
  | exception Enforest.Error "operator infix associativity must be Left or Right" -> ()
  | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
  | _ -> Alcotest.fail "expected invalid operator infix associativity to be rejected"

let syntax_extension_circular_visit () =
  with_modules
    [ ("a", "pub syntax aop { | aop $x => { import \"b\"; $x } }");
      ("b", "pub syntax bop { | bop $x => { import \"a\"; $x } }") ]
    (fun loader ->
      match
        parse_with_macros
          ~load_syntax:(Core_loader.load_syntax_exports loader)
          "import \"a\""
      with
      | exception Core_loader.CircularSyntaxVisit "a" -> ()
      | exception e -> Alcotest.fail ("unexpected exception: " ^ Printexc.to_string e)
      | _ -> Alcotest.fail "expected circular syntax visit")

(* Brace surface syntax: removed forms fail naming the new one; [=>] is
   reserved; a trailing [;] discards a block's value. *)
let rejected_with needle parse_fn source () =
  match parse_fn source with
  | exception (Enforest.Error msg | Enforest.Unsupported msg) ->
      if not (string_contains msg needle) then Alcotest.fail ("unexpected message: " ^ msg)
  | _ -> Alcotest.fail ("expected rejection: " ^ source)

let trailing_semicolon_discards () =
  match parse "{ x = 1; x + 1; }" with
  | Let { body = Let { name = "_"; body = Atom Atom.Unit; _ }; _ } -> ()
  | _ -> Alcotest.fail "expected the block's value to be discarded"

let no_trailing_semicolon_keeps_value () =
  match parse "{ x = 1; x + 1 }" with
  | Let { body = Ap _; _ } -> ()
  | _ -> Alcotest.fail "expected the block's value to be its last expression"

let newline_is_whitespace () =
  match parse "{ x = 1;\n  x\n  + 1 }" with
  | Let { body = Ap _; _ } -> ()
  | _ -> Alcotest.fail "expected a newline not to end the expression"

let brace_syntax_suite =
  [ Alcotest.test_case "-> body rejected" `Quick (rejected_with "-> body was removed" parse "fn(x) -> x");
    Alcotest.test_case "do block rejected" `Quick (rejected_with "blocks were removed" parse "do 1 end");
    Alcotest.test_case "match without parens rejected" `Quick (rejected_with "match (scrutinee)" parse "match x { | _ => 1 }");
    Alcotest.test_case "match arm needs =>" `Quick (rejected_with "=>" parse "match (x) { | _ -> 1 }");
    Alcotest.test_case "=> is reserved" `Quick (rejected_with "reserved" parse_module "infix (=>) 3 Left");
    Alcotest.test_case "record type needs struct" `Quick (rejected_with "struct {" parse_module "type P = {x: I64}");
    Alcotest.test_case "trailing ; discards" `Quick trailing_semicolon_discards;
    Alcotest.test_case "no trailing ; keeps value" `Quick no_trailing_semicolon_keeps_value;
    Alcotest.test_case "newline is whitespace" `Quick newline_is_whitespace;
    Alcotest.test_case "empty block rejected" `Quick (rejected_with "empty block" parse "{ }");
  ]

let suites =
  [ ( "brace syntax", brace_syntax_suite );
    ( "enforest",
      [ Alcotest.test_case "raw grouping" `Quick raw_grouping;
        Alcotest.test_case "comments" `Quick line_and_block_comments;
        Alcotest.test_case "datum comment" `Quick datum_comment;
        Alcotest.test_case "$ token is separate" `Quick dollar_token_is_separate;
        Alcotest.test_case "strict phase rule: operators need std" `Quick strict_phase_rule_operators_need_std;
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
        Alcotest.test_case "if { else }" `Quick if_do_else_end;
        Alcotest.test_case "match { }" `Quick match_do_end;
        Alcotest.test_case "ref call deref" `Quick ref_call_deref;
        Alcotest.test_case "module newline separators" `Quick module_newline_separators;
        Alcotest.test_case "resume unit call" `Quick resume_unit_call;
        Alcotest.test_case "import shape" `Quick import_shape;
        Alcotest.test_case "open in do block" `Quick open_in_do_block;
        Alcotest.test_case "module-level open shape" `Quick module_level_open_shape;
        Alcotest.test_case "module-level open of import shape" `Quick module_level_open_import_shape;
        Alcotest.test_case "struct-level open shape" `Quick struct_level_open_shape;
        Alcotest.test_case "module-level pub open rejected" `Quick module_level_pub_open_rejected;
        Alcotest.test_case "module without open has no prelude operators" `Quick
          module_without_open_has_no_prelude_operators;
        Alcotest.test_case "module open scopes later statements only" `Quick
          module_open_scopes_later_statements_only;
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
        Alcotest.test_case "operator prefix in do block" `Quick operator_prefix_in_do_block;
        Alcotest.test_case "operator prefix simple do" `Quick operator_prefix_simple_do;
        Alcotest.test_case "operator infix in do block" `Quick operator_infix_in_do_block;
        Alcotest.test_case "operator prefix in module" `Quick operator_prefix_in_module;
        Alcotest.test_case "operator RHS can use earlier macro" `Quick operator_rhs_can_use_earlier_macro;
        Alcotest.test_case "operator declaration is sequential" `Quick operator_declaration_is_sequential;
        Alcotest.test_case "duplicate operator later wins" `Quick duplicate_operator_later_wins;
        Alcotest.test_case "syntax extension cross module" `Quick syntax_extension_cross_module;
        Alcotest.test_case "syntax import does not leak between parses" `Quick syntax_import_does_not_leak_between_parses;
        Alcotest.test_case "syntax extension not runtime field" `Quick syntax_extension_not_runtime_field;
        Alcotest.test_case "pub syntax rejected in struct" `Quick pub_syntax_rejected_in_struct;
        Alcotest.test_case "syntax exports include operator metadata" `Quick syntax_exports_include_operator_metadata;
        Alcotest.test_case "duplicate public syntax exports rejected" `Quick duplicate_public_syntax_exports_rejected;
        Alcotest.test_case "syntax branch head mismatch rejected" `Quick syntax_branch_head_mismatch_rejected;
        Alcotest.test_case "syntax unbound replacement hole rejected" `Quick syntax_unbound_replacement_hole_rejected;
        Alcotest.test_case "syntax Id hole binds and refers" `Quick syntax_id_hole_binds_and_refers;
        Alcotest.test_case "syntax old hole kind spelling rejected" `Quick syntax_old_hole_kind_spelling_rejected;
        Alcotest.test_case "syntax pattern hole expression position rejected" `Quick syntax_pattern_hole_expression_position_rejected;
        Alcotest.test_case "syntax multi rejected" `Quick syntax_multi_rejected;
        Alcotest.test_case "syntax postfix rejected in module" `Quick syntax_postfix_rejected_in_module;
        Alcotest.test_case "syntax postfix rejected in do block" `Quick syntax_postfix_rejected_in_do_block;
        Alcotest.test_case "old syntax declaration rejected" `Quick old_syntax_declaration_rejected;
        Alcotest.test_case "operator infix bad assoc rejected" `Quick operator_infix_bad_assoc_rejected;
        Alcotest.test_case "syntax extension circular visit" `Quick syntax_extension_circular_visit;
      ] );
  ]
