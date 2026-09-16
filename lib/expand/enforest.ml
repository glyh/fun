exception Unsupported = Enforest_util.Unsupported
exception Error = Enforest_util.Error

open Raw_syntax
open Enforest_util

let parse_pat_terms = Enforest_pat.parse_pat_terms

let rec parse_args env terms =
  match split_commas terms with
  | [ [] ] -> []
  | parts ->
      List.map
        (fun part ->
          if List.for_all is_separator part then error "empty argument";
          parse_all (fun ts -> parse_expr_prec env Top ts) part)
        parts

(* A call's arguments, exactly as written, read as the kinds of the macro its
   head names (M8, M9); [None] when the head names no macro the reader knows,
   and the call is an application. An empty [()] is the one [()] argument an
   empty parameter list declares, as for a function. *)
and macro_call_args env (head : Syntax.t) items =
  match env.macro_params head with
  | Some kinds ->
      let macro = match head.kind with Syntax.Var id -> id.name | FieldAccess (_, f) -> f | _ -> "_" in
      let parts = match drop_separators items with [] -> [ [] ] | items -> split_commas items in
      if List.length parts <> List.length kinds then
        Expand_error.raise_at (ArgumentCount { macro; expected = List.length kinds; got = List.length parts });
      Some
        (List.map2
           (fun kind part ->
             match (kind : Syntax.hole_kind), drop_separators part with
             | HoleExpr, [] when drop_separators items = [] -> Syntax.CapExpr (unit ())
             | HoleExpr, _ -> Syntax.CapExpr (parse_all (fun ts -> parse_expr_prec env Top ts) part)
             | HoleId, [ { datum = Token ({ kind = Ident _ | Operator _; _ } as tok); _ } ] -> Syntax.CapId tok
             | HoleBlock, [ ({ datum = Group (Raw_syntax.Brace, ts, _); _ } as group) ] ->
                 if env.eager then Syntax.CapExpr (parse_all (fun ts -> parse_expr_prec env Top ts) [ group ])
                 else Syntax.CapBlock ts
             | HolePattern, _ :: _ -> Syntax.CapPattern (parse_pat_terms part)
             (* Declarations, captured unread as a syntax form's are: read where
                they are spliced. *)
             | HoleDecl, [ { datum = Group (Raw_syntax.Brace, ts, _); _ } ] ->
                 Syntax.CapDecls (if drop_separators ts = [] then [] else [ Syntax.Items ts ])
             (* Exactly one declaration, written as a group holding one item. *)
             | HoleOneDecl, [ { datum = Group (Raw_syntax.Brace, ts, _); _ } ]
               when List.length (List.filter (fun s -> drop_separators s <> []) (split_statements ts)) = 1 ->
                 Syntax.CapDecl (Syntax.Items ts)
             (* The argument's tokens, unread: the macro reads them itself. *)
             | HoleTokens, _ -> Syntax.CapTokens (drop_separators part)
             | (HoleId | HoleBlock | HolePattern | HoleDecl | HoleOneDecl), _ ->
                 Expand_error.raise_at (ArgumentKind { macro; kind; span = syntax_span part }))
           kinds parts)
  | _ -> None

and parse_group_expr env delimiter items span =
  match delimiter with
  | Raw_syntax.Paren -> (
      let items = drop_separators items in
      match items with
      | [] -> unit ~span ()
      | [ ({ datum = Token { kind = Operator name; _ }; _ } as term) ] -> { (var_of term name) with span }
      | _ -> (
          match split_at_token Colon items with
          | Some (expr_terms, _, typ_terms) ->
              stx ~span
                (Syntax.Annotated
                   {
                     inner =
                       parse_all (fun ts -> parse_expr_prec env Top ts) expr_terms;
                     typ = parse_type_terms env typ_terms;
                   })
          | None -> (
              match split_commas items with
              | [ only ] ->
                  {
                    (parse_all (fun ts -> parse_expr_prec env Top ts) only) with
                    span;
                  }
              | parts ->
                  let exprs =
                    List.map
                      (parse_all (fun ts -> parse_expr_prec env Top ts))
                      parts
                  in
                  stx ~span (Syntax.Prod exprs))))
  | Bracket -> unsupported "bare bracket expression is not in Phase 7A"
  | Brace -> parse_block env span items

(* A [{ … }] body: read when expansion reaches it (M9), or now, as quoted syntax. *)
and parse_block env span items =
  if env.eager then parse_do_body_terms env span items else stx ~span (Syntax.Block items)

and parse_record_expr_fields env items =
  split_statements items
  |> List.map (fun part ->
      match drop_separators part with
      | { datum = Token { kind = Ident name; _ }; _ } :: eq :: value_terms
        when token_kind Equals eq ->
          (name, parse_all (fun ts -> parse_expr_prec env Top ts) value_terms)
      | _ -> error "expected record field of the form name = expr")

and parse_type_terms env terms = parse_all (parse_type_entry env) terms

(* A type is an expression (types are values): read with the one grammar. *)
and parse_type_entry env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  parse_expr_prec env Top terms

and parse_param_item env explicitness terms =
  let terms = drop_separators terms in
  match terms with
  | [] when explicitness = Explicitness.Explicit ->
      param ~type_:(unit_type ()) Explicitness.Explicit "_"
  | [ ({ datum = Token { kind = Ident name; _ }; _ } as term) ] ->
      param_id explicitness (id_of term name)
  (* [[A : {Eq, Show}]]: an implicit binder's bound set. *)
  | ({ datum = Token { kind = Ident name; _ }; _ } as term) :: colon :: [ { datum = Group (Raw_syntax.Brace, items, span); _ } ]
    when token_kind Colon colon && explicitness = Explicitness.Implicit ->
      let bounds = List.map (fun ts -> parse_all (fun ts -> parse_expr_prec env Top ts) ts) (split_commas (drop_separators items)) in
      param_id ~type_:(stx ~span (Syntax.TraitBoundSet bounds)) explicitness (id_of term name)
  | ({ datum = Token { kind = Ident name; _ }; _ } as term) :: colon :: typ_terms
    when token_kind Colon colon ->
      param_id ~type_:(parse_type_terms env typ_terms) explicitness (id_of term name)
  | _ -> error "expected parameter of the form name or name : Type"

and parse_param_group env explicitness items =
  let items = drop_separators items in
  match items with
  | [] when explicitness = Explicitness.Explicit ->
      [ param ~type_:(unit_type ()) Explicitness.Explicit "_" ]
  | [] -> error "empty implicit parameter list"
  | _ -> List.map (parse_param_item env explicitness) (split_commas items)

and parse_fn_parts env ?(allow_empty = false) ?(kind_annotation = false)
    start_span terms =
  let terms = drop_separators terms in
  let implicit_params, rest =
    match terms with
    | ({ datum = Group (Raw_syntax.Bracket, items, _); _ } as group) :: rest ->
        require_adjacent_span start_span group.span "implicit fn parameter list";
        (parse_param_group env Explicitness.Implicit items, rest)
    | rest -> ([], rest)
  in
  let explicit_params, rest =
    match drop_separators rest with
    | ({ datum = Group (Raw_syntax.Paren, items, _); _ } as group) :: rest ->
        let previous_span =
          match terms with
          | ({ datum = Group (Raw_syntax.Bracket, _, _); _ } as g) :: _ ->
              g.span
          | _ -> start_span
        in
        require_adjacent_span previous_span group.span
          "explicit fn parameter list";
        (parse_param_group env Explicitness.Explicit items, rest)
    | rest when implicit_params <> [] || allow_empty -> ([], rest)
    | _ -> error "fn requires at least one parameter list"
  in
  let params = implicit_params @ explicit_params in
  (* A macro's annotation: [: Decl], or [: Expr(T)] whose [T] is the type its
     output promises - elaborated where the macro is defined, as its signature.
     [: Expr(_)] promises nothing. *)
  let kind, output, rest =
    if not kind_annotation then (None, None, rest)
    else
      match drop_separators rest with
      | { datum = Token { kind = Colon; _ }; _ }
        :: { datum = Token { kind = Ident "Expr"; _ }; _ }
        :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest -> (
          match drop_separators items with
          | [ { datum = Token { kind = Ident "_"; _ }; _ } ] -> (Some Syntax.MacroAnnotation.Expr, None, rest)
          | items ->
              let t, t_rest = parse_expr_prec env Top items in
              ensure_no_rest "macro annotation" t_rest;
              (Some Syntax.MacroAnnotation.Expr, Some t, rest))
      (* [: Decl] returns one declaration, [: List(Decl)] any number: the type its
         body is checked against, [Decl] written as the prelude's [Syntax.Decl]
         at the annotation's own scopes. *)
      | { datum = Token { kind = Colon; _ }; _ } :: ({ datum = Token { kind = Ident "Decl"; _ }; _ } as decl) :: rest ->
          (Some Syntax.MacroAnnotation.Decl, Some (syntax_decl_type decl), rest)
      | { datum = Token { kind = Colon; _ }; _ }
        :: ({ datum = Token { kind = Ident "List"; _ }; _ } as list)
        :: { datum = Group (Raw_syntax.Paren, items, _); span } :: rest
        when (match drop_separators items with [ { datum = Token { kind = Ident "Decl"; _ }; _ } ] -> true | _ -> false) ->
          let decl = List.hd (drop_separators items) in
          let list_ty = stx ~span:list.span (Syntax.Var (id_of list "List")) in
          (Some Syntax.MacroAnnotation.Decl, Some (stx ~span (Syntax.Ap (list_ty, Explicitness.Explicit, syntax_decl_type decl))), rest)
      | { datum = Token { kind = Colon; _ }; _ } :: _ ->
          error "a macro annotation is : Expr(T), : Expr(_), : Decl or : List(Decl)"
      | rest -> (None, None, rest)
  in
  (* A macro's type binders are solved before it runs, each handed to it as the
     reflected type it was solved to - a [Syntax.R] unless annotated. *)
  let params =
    if not kind_annotation then params
    else
      match implicit_params, kind with
      | [], _ -> params
      | _, Some Syntax.MacroAnnotation.Decl -> error "a Decl macro binds no type parameter"
      | _ ->
          List.map
            (fun (p : Syntax.param) ->
              (* Written at the binder, so it carries the binder's scopes. *)
              let syntax_id = Syntax.fresh_id ~span:p.name.span ~scope:p.name.scope Compiler_names.Module_name.syntax in
              let r_type = stx (Syntax.FieldAccess (stx (Syntax.Var syntax_id), Compiler_names.Syntax_name.r)) in
              { p with type_ = Some (Option.value p.type_ ~default:r_type) })
            implicit_params
          @ explicit_params
  in
  (* [fn(x) : T { … }]: the result type, the body checked against it; an
     effectful result [fn(x : A) ->{E} T { … }] / [~> T] annotates the whole
     function with its arrow type. A macro's [:] is its annotation, above. *)
  let result, rest = if kind_annotation then (None, rest) else parse_result_type env rest in
  let body, rest, span = parse_body env "fn parameters" rest in
  let span = span_between start_span span in
  (params, kind, output, result, body, rest, span)

(* An optional result before a body: [: T] when pure, [->{E} T] or [~> T] when
   effectful - the result's type and its row. Brackets decide grouping: the type
   ends at the first top-level [{ … }], so a type holding braces is
   parenthesised ([: (struct { x : I64 })]). *)
and parse_result_type env terms =
  let result_type what rest =
    let ends term = match term.datum with Group (Raw_syntax.Brace, _, _) -> true | _ -> false in
    let rec split acc = function
      | term :: _ as rest when ends term -> (List.rev acc, rest)
      | term :: rest -> split (term :: acc) rest
      | [] -> (List.rev acc, [])
    in
    let type_terms, rest = split [] rest in
    if drop_separators type_terms = [] then error ("expected a result type after " ^ what);
    (parse_all (fun ts -> parse_expr_prec env Top ts) type_terms, rest)
  in
  match drop_separators terms with
  | colon :: rest when token_kind Colon colon ->
      let typ, rest = result_type ":" rest in
      (Some (typ, None), rest)
  | arrow :: { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest
    when token_kind ThinArrow arrow && spans_adjacent arrow.span span ->
      let row = parse_effect_row_terms env items in
      let typ, rest = result_type "->{…}" rest in
      (Some (typ, Some row), rest)
  | arrow :: _ when token_kind ThinArrow arrow -> error "a pure result is written : T; ->{E} T is for an effectful one"
  | arrow :: rest when is_poly_arrow env arrow ->
      let typ, rest = result_type "~>" rest in
      (Some (typ, Some { Syntax.effects = []; tails = []; inferred = true; polymorphic = true }), rest)
  | _ -> (None, terms)

(* [: T] is the pure member of the result-type family: the body is checked at
   [T] and the definition's row is empty, so a body that performs must say so
   with [->{E} T] or [~> T]. The annotation therefore rides on the function's
   type, as an effectful result's does - unless a parameter has no type to write
   there, when only the body's type can be stated. *)
and result_row = function
  | Some (typ, Some row) -> Some (typ, row)
  | Some (typ, None) -> Some (typ, { Syntax.effects = []; tails = []; inferred = false; polymorphic = false })
  | None -> None

(* A body is a brace group, parsed as a block. *)
and parse_body env what terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest -> (parse_block env span items, rest, span)
  (* A [Block] hole stands for a [{ … }] in quoted syntax (M9). *)
  | ({ datum = Token _; span } as hole) :: rest when env.eager && Option.is_some (Enforest_template.hole_ident hole) ->
      (stx ~span (Syntax.Block [ hole ]), rest, span)
  | term :: _ when token_kind ThinArrow term -> error ("expected { body } after " ^ what ^ "; -> body was removed, write { … }")
  | term :: _ when token_kind KwDo term -> error ("expected { body } after " ^ what ^ "; do … end was removed, write { … }")
  | _ -> error ("expected { body } after " ^ what)

and parse_fn ?(kind_annotation = false) env start_span terms =
  let params, kind, output, result, body, rest, span =
    parse_fn_parts ~kind_annotation env start_span terms
  in
  let lam = List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body in
  let value =
    match result_row result with
    | Some (typ, row) -> stx ~span (Syntax.Annotated { inner = lam; typ = function_type ~span params row typ })
    | None -> lam
  in
  ((kind, output), value, rest)

(* [fn(p1 : A, …) ->{E} T]'s type: its parameters' arrows, the last carrying the row. *)
and function_type ~span (params : Syntax.param list) row typ =
  let rec go = function
    | [] -> error "a result type needs a parameter list"
    | (p : Syntax.param) :: rest ->
        let dom =
          match p.type_ with
          | Some t -> t
          | None -> error ("a result type needs every parameter's type: " ^ p.name.name)
        in
        let row, cod = match rest with [] -> (Some row, typ) | _ -> (None, go rest) in
        stx ~span (Syntax.Arrow (p.explicitness, Some p.name, dom, row, cod))
  in
  go params

and parse_method_params env items =
  let items = drop_separators items in
  if items = [] then [] else parse_param_group env Explicitness.Explicit items

and parse_method_binding env public stmt =
  let header =
    Parse_spec.seq (Parse_spec.punct KwMethod) Parse_spec.str_ident
  in
  match Parse_spec.parse header env stmt with
  | Some (((), (name, name_term)), rest) -> (
      match Enforest_util.drop_separators rest with
      | ({ datum = Group (Raw_syntax.Paren, items, _); _ } as params_group)
        :: rest ->
          require_adjacent_span name_term.span params_group.span
            "method parameter list";
          let params = parse_method_params env items in
          (* [: T] when pure, [->{E} T] when not: a method is pure unless it
             declares a row (E3). *)
          let result, rest = parse_result_type env rest in
          let effects = match result with Some (_, row) -> row | None -> None in
          let body, rest, _ = parse_body env "method parameters" rest in
          let body = match result with Some (typ, _) -> stx ~span:body.span (Syntax.Annotated { inner = body; typ }) | None -> body in
          ensure_no_rest "method declaration" rest;
          Some
            (Syntax.MethodBinding
               { name = id_of name_term name; params; effects; body; public })
      | _ ->
          error
            ("method declaration requires a parenthesized parameter list: "
           ^ name))
  | None -> None

and parse_module_type_fields env what terms =
  Enforest_decl_helpers.parse_module_type_fields (parse_type_terms env) what
    terms

and parse_effect_ops env op_terms =
  Enforest_decl_helpers.parse_effect_ops (parse_type_terms env) op_terms

and parse_trait_fields env field_terms =
  Enforest_decl_helpers.parse_trait_fields (parse_type_terms env) field_terms

and parse_decl_type_params what name_span param_terms =
  Enforest_decl_helpers.parse_decl_type_params what name_span param_terms

and form_callbacks env =
  {
    Enforest_forms.parse_expr_prec = parse_expr_prec env;
    parse_expr_terms =
      (fun ts -> parse_all (fun ts -> parse_expr_prec env Top ts) ts);
    parse_do_body_terms = parse_do_body_terms env;
    parse_pat_terms;
    parse_items = parse_module_items env;
    is_expr_start = is_expr_start env;
  }


and parse_match env start_span terms =
  Enforest_forms.parse_match (form_callbacks env) start_span terms

and parse_group_arg env items =
  Enforest_forms.parse_group_arg (form_callbacks env) items

and parse_effect_row_terms env terms =
  Enforest_forms.parse_effect_row_terms (form_callbacks env) terms


and parse_ref env start_span terms =
  Enforest_forms.parse_ref (form_callbacks env) start_span terms

and parse_deref env start_span terms =
  Enforest_forms.parse_deref (form_callbacks env) start_span terms

and parse_resume env start_span terms =
  Enforest_forms.parse_resume (form_callbacks env) start_span terms

and parse_perform env start_span terms =
  Enforest_forms.parse_perform (form_callbacks env) start_span terms

and parse_import _env (kw : Raw_syntax.t) terms =
  Enforest_forms.parse_import ~scope:(token_scope kw) kw.span terms

(* A module's items: read one form at a time as expansion reaches them (M9),
   or now, as quoted syntax. *)
and parse_module_items env body_terms =
  if env.eager then parse_module_bindings env body_terms else [ Syntax.Items body_terms ]

and parse_module_expr env start_span terms =
  match drop_separators terms with
  | ({ datum = Token _; span } as hole) :: rest when env.eager && Option.is_some (Enforest_template.hole_ident hole) ->
      (stx ~span:(span_between start_span span) (Syntax.Module { bindings = [ Syntax.Items [ hole ] ] }), rest)
  | _ ->
      let body_terms, rest, span = brace_body "module" terms in
      let bindings = parse_module_items env body_terms in
      (stx ~span:(span_between start_span span) (Syntax.Module { bindings }), rest)

and parse_sig_expr env start_span terms =
  let body_terms, rest, span = brace_body "sig" terms in
  let bindings =
    split_statements body_terms
    |> List.map (fun stmt ->
        match drop_separators stmt with
        (* [ord_T : impl Ord(T)]: a named impl the module must provide. *)
        | ({ datum = Token { kind = Ident name; _ }; _ } as name_term)
          :: colon :: { datum = Token { kind = KwImpl; _ }; _ } :: trait_terms
          when token_kind Colon colon ->
            let trait, args = parse_impl_trait env trait_terms in
            Syntax.ImplBinding { name = Some (id_of name_term name); trait; args; fields = []; public = true }
        | { datum = Token { kind = KwImpl; _ }; _ } :: _ ->
            error "an impl in a signature must be named: write name : impl Trait(Type)"
        | ({ datum = Token { kind = Ident name; _ }; _ } as name_term)
          :: colon :: typ_terms
          when token_kind Colon colon ->
            Syntax.LetBinding
              {
                name = id_of name_term name;
                value = parse_type_terms env typ_terms;
                public = true;
                recursive = false;
              }
        | _ -> error "expected signature field name : type")
  in
  (stx ~span:(span_between start_span span) (Syntax.Sig { bindings }), rest)

(* [enum { Red, Some(A), Pair(A, B) }]: constructors separated by commas; a
   constructor's payloads are its parenthesised, comma-separated types. *)
and parse_enum_expr env start_span terms =
  let body_terms, rest, span = brace_body "enum" terms in
  let ctors =
    split_commas (drop_separators body_terms)
    |> List.filter (fun item -> drop_separators item <> [])
    |> List.map (fun item ->
        match drop_separators item with
        | [ { datum = Token { kind = Ident cname; _ }; _ } ] -> (cname, [])
        | [ { datum = Token { kind = Ident cname; _ }; _ }; { datum = Group (Raw_syntax.Paren, items, _); _ } ] ->
            (cname, List.map (parse_type_terms env) (List.filter (fun ts -> drop_separators ts <> []) (split_commas (drop_separators items))))
        | _ -> error "an enum constructor is written Name or Name(Type, …)")
  in
  (stx ~span:(span_between start_span span) (Syntax.Enum { name = None; ctors }), rest)

and parse_struct_expr env start_span terms =
  let body_terms, rest, span = brace_body "struct" terms in
  let bindings = parse_struct_items env body_terms in
  (stx ~span:(span_between start_span span) (Syntax.Struct { bindings }), rest)

and parse_primary env terms =
  match drop_separators terms with
  | [] -> error "expected expression"
  | term :: rest -> (
      match term.datum with
      | Token { kind = Int n; _ } -> (atom ~span:term.span (Atom.I64 n), rest)
      | Token { kind = String s; _ } ->
          (atom ~span:term.span (Atom.String s), rest)
      | Token { kind = Char c; _ } -> (atom ~span:term.span (Atom.Char c), rest)
      | Token { kind = Unit; _ } -> (unit ~span:term.span (), rest)
      | Token { kind = KwUnit; _ } -> (var_of term "Unit", rest)
      | Token { kind = KwSelf; _ } -> (stx ~span:term.span Syntax.Self, rest)
      | Token { kind = KwSelfType; _ } ->
          (stx ~span:term.span Syntax.SelfType, rest)
      | Token { kind = KwLet; _ } ->
          error "let ... in syntax is not supported; use do blocks and bindings"
      | Token { kind = KwFun; _ } -> error "fun syntax is not supported; use fn"
      | Token { kind = KwMacro; _ } ->
          error
            "macro ... in syntax is not supported; use macro declarations in \
             do blocks or modules"
      | Token { kind = KwTrait; _ } ->
          error
            "trait ... in syntax is not supported; use do blocks and trait \
             declarations"
      | Token { kind = KwImpl; _ } ->
          error
            "impl ... in syntax is not supported; use do blocks and impl \
             declarations"
      | Token { kind = KwDo; _ } -> error "do … end blocks were removed; write { … }"
      | Token { kind = KwFn; _ } ->
          let _, expr, rest = parse_fn env term.span rest in
          (expr, rest)
      | Token { kind = KwMatch; _ } -> parse_match env term.span rest
      | Token { kind = KwRef; _ } -> parse_ref env term.span rest
      | Token { kind = KwDeref; _ } -> parse_deref env term.span rest
      | Token { kind = KwResume; _ } -> parse_resume env term.span rest
      | Token { kind = KwPerform; _ } -> parse_perform env term.span rest
      | Token { kind = KwImport; _ } -> parse_import env term rest
      | Token { kind = KwModule; _ } -> parse_module_expr env term.span rest
      | Token { kind = KwSig; _ } -> parse_sig_expr env term.span rest
      | Token { kind = KwStruct; _ } -> parse_struct_expr env term.span rest
      | Token { kind = KwEnum; _ } -> parse_enum_expr env term.span rest
      | Token { kind = Ident "quote"; _ }
        when (match drop_separators rest with { datum = Group ((Raw_syntax.Paren | Raw_syntax.Brace), _, _); _ } :: _ -> true | _ -> false) ->
          Enforest_forms.parse_quote (fun holes -> form_callbacks (eager_env ~holes env)) term.span rest
      | Token { kind = Ident name | Operator name; _ } -> (
          match Binding.find_role env.operators ~fixity:Syntax.PrefixOp ~scope:(token_scope term) name with
          | Some ({ meaning = Syntax.Rules { rules_kind; rules }; from_unit; _ } as role) ->
              let inst, rest =
                Enforest_template.instantiate (template_callbacks env (Operand (name, role))) ~form:(id_of term name) ~kind:rules_kind
                  ~position:Syntax.MacroAnnotation.Expr ~from_unit rules (term :: rest)
              in
              (stx ~span:term.span (Syntax.Instantiate inst), rest)
          | Some role ->
              let rhs, rest = parse_expr_prec env (Operand (name, role)) rest in
              let span = span_between term.span rhs.span in
              let f = var_of term name in
              let expr =
                match role.meaning with
                | Syntax.CallMacro ->
                    stx ~span (Syntax.MacroCall (f, [ Syntax.CapExpr (syntax_operator_arg ~span ~use:term name role [ rhs ]) ]))
                | _ -> ap ~span f Explicitness.Explicit rhs
              in
              (expr, rest)
          | None -> (
              match term.datum with
              | Token { kind = Operator _; _ } -> unsupported ("unsupported prefix operator: " ^ name)
              | _ -> (var_of term name, rest)))
      | Token { kind = Eof; _ } -> error "unexpected EOF in expression"
      | Token { kind; _ } -> (
          match keyword_name kind with
          | Some name -> unsupported ("unsupported Phase 7A keyword: " ^ name)
          | None -> (
              match punct_name kind with
              | Some name ->
                  error ("unexpected punctuation in expression: " ^ name)
              | None -> error "unexpected token in expression"))
      | Group (Raw_syntax.Bracket, items, span) -> (
          match drop_separators rest with
          | arrow :: cod_terms when token_kind ThinArrow arrow ->
              let params = parse_param_group env Explicitness.Implicit items in
              let cod : Syntax.t =
                parse_all (fun ts -> parse_expr_prec env Top ts) cod_terms
              in
              let result =
                List.fold_right
                  (fun (p : Syntax.param) (acc : Syntax.t) ->
                    let dom = Option.value ~default:(unit_type ()) p.type_ in
                    stx
                      ~span:(span_between span acc.span)
                      (Syntax.Arrow
                         (Explicitness.Implicit, Some p.name, dom, None, acc)))
                  params cod
              in
              (result, [])
          | _ -> (parse_group_expr env Raw_syntax.Bracket items span, rest))
      | Group (delimiter, items, span) ->
          (parse_group_expr env delimiter items span, rest))

and parse_expr_prec env prec terms =
  let lhs, rest = parse_primary env terms in
  parse_postfix_infix env prec lhs rest

(* Whether infix operator [symbol] continues an expression read at [prec]: at
   an operand, only if it binds tighter than the operator the operand belongs
   to - by their groups' declared order, never a guess. *)
and continues prec symbol (role : Syntax.role) =
  let no_order outer =
    error (Printf.sprintf "`%s` and `%s` have no declared order; parenthesise one of them" outer symbol)
  in
  match prec with
  | Top | ArrowRhs -> true
  | Tight -> false
  | Operand (outer, outer_role) -> (
      match outer_role.order, role.order with
      | Some o, Some i -> (
          match Syntax.order_relation i o with
          | Syntax.Stronger -> true
          | Weaker -> false
          | Same -> (
              match o.group_assoc with
              | Syntax.RightAssoc -> true
              | LeftAssoc -> false
              | NonAssoc ->
                  error
                    (Printf.sprintf "`%s` and `%s` do not chain: their group %s is assoc(none); parenthesise one of them"
                       outer symbol o.group_name))
          | Unrelated -> no_order outer)
      | Some _, None -> false
      | None, Some _ -> true
      | None, None -> no_order outer)

and parse_postfix_infix env min_prec lhs terms =
  match terms with
  | term :: _ when is_separator term -> (lhs, terms)
  | term :: rest when (token_kind ThinArrow term || is_poly_arrow env term) && (match min_prec with Top | ArrowRhs -> true | _ -> false) ->
      (* [A ->{E} B]: a brace group adjacent to the arrow is its effect row.
         [A ~> B]: a polymorphic row, decided by the arrow's signature. *)
      let row, rest =
        match rest with
        | _ when not (token_kind ThinArrow term) ->
            (Some { Syntax.effects = []; tails = []; inferred = false; polymorphic = true }, rest)
        | { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest when spans_adjacent term.span span ->
            (Some (parse_effect_row_terms env items), rest)
        | _ -> (None, rest)
      in
      let rhs, rest = parse_expr_prec env ArrowRhs rest in
      let span = span_between lhs.span rhs.span in
      let lhs =
        match lhs.kind with
        | Syntax.Annotated { inner = { kind = Syntax.Var name; _ }; typ } ->
            stx ~span
              (Syntax.Arrow (Explicitness.Explicit, Some name, typ, row, rhs))
        | _ ->
            stx ~span
              (Syntax.Arrow (Explicitness.Explicit, None, lhs, row, rhs))
      in
      parse_postfix_infix env min_prec lhs rest
  | term :: rest when token_kind Colon term ->
      let typ, rest = parse_type_entry env rest in
      let lhs =
        stx
          ~span:(span_between lhs.span typ.span)
          (Syntax.Annotated { inner = lhs; typ })
      in
      parse_postfix_infix env min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Paren, items, span); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "function call";
      let call_span = span_between lhs.span term.span in
      let lhs =
        match macro_call_args env lhs items with
        | Some args -> stx ~span:call_span (Syntax.MacroCall (lhs, args))
        | None ->
            let args =
              match drop_separators items with
              | [] -> [ unit ~span () ]
              | _ -> parse_args env items
            in
            List.fold_left
              (fun f arg -> ap ~span:call_span f Explicitness.Explicit arg)
              lhs args
      in
      parse_postfix_infix env min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Bracket, items, _); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "implicit argument list";
      let args = parse_args env items in
      let call_span = span_between lhs.span term.span in
      let lhs =
        List.fold_left
          (fun f arg -> ap ~span:call_span f Explicitness.Implicit arg)
          lhs args
      in
      parse_postfix_infix env min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Brace, items, _); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "record construction";
      let items = drop_separators items in
      let call_span = span_between lhs.span term.span in
      let lhs =
        if List.exists (token_kind Equals) items then
          stx ~span:call_span
            (Syntax.RecordConstruct
               { typ = lhs; fields = parse_record_expr_fields env items })
        else
          let arg = parse_all (fun ts -> parse_expr_prec env Top ts) items in
          ap ~span:call_span lhs Explicitness.Implicit arg
      in
      parse_postfix_infix env min_prec lhs rest
  | term :: field :: rest when token_kind Dot term -> (
      match field.datum with
      | Token { kind = Int i; _ } ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.Proj (lhs, Int64.to_int i)) in
          parse_postfix_infix env min_prec lhs rest
      | _ -> (
          match token_text field with
          | Some name ->
              let span = span_between lhs.span field.span in
              let lhs = stx ~span (Syntax.FieldAccess (lhs, name)) in
              parse_postfix_infix env min_prec lhs rest
          | None -> error "expected field name or projection after '.'"))
  | term :: rest -> (
      match token_text term with
      | Some symbol -> (
          match Binding.find_role env.operators ~fixity:Syntax.InfixOp ~scope:(token_scope term) symbol with
          | Some role when continues min_prec symbol role ->
              let rhs, rest = parse_expr_prec env (Operand (symbol, role)) rest in
              let span = span_between lhs.span rhs.span in
              let lhs =
                match role.meaning with
                | Syntax.AssignRef -> stx ~span (Syntax.RefSet (lhs, rhs))
                | Syntax.Rules { rules = [ rule ]; _ } ->
                    let captures =
                      match List.rev (Enforest_template.pattern_holes rule.pattern) with
                      | [ l; r ] -> [ (l, Syntax.CapExpr lhs); (r, Syntax.CapExpr rhs) ]
                      | _ -> error ("an infix syntax form takes two operands: " ^ symbol)
                    in
                    stx ~span (Syntax.Instantiate { form = id_of term symbol; rule; captures; from_unit = role.from_unit })
                | Syntax.Rules _ -> error ("an infix syntax form has one rule: " ^ symbol)
                | Syntax.CallMacro -> syntax_operator_arg ~span ~use:term symbol role [ lhs; rhs ]
                | Syntax.ApplyValue ->
                    ap ~span (ap ~span (var_of term symbol) Explicitness.Explicit lhs) Explicitness.Explicit rhs
                | Syntax.OrderGroup | Syntax.PolyArrow -> error ("not an infix operator: " ^ symbol)
              in
              parse_postfix_infix env min_prec lhs rest
          | _ -> (lhs, term :: rest))
      | None -> (lhs, term :: rest))
  | [] -> (lhs, [])

and is_poly_arrow env term =
  match token_text term with
  | Some symbol -> (
      match Binding.find_role env.operators ~fixity:Syntax.InfixOp ~scope:(token_scope term) symbol with
      | Some { meaning = Syntax.PolyArrow; _ } -> true
      | _ -> false)
  | None -> false

and parse_binding_statement env stmt =
  match parse_value_decl_statement env stmt with
  | Some decl -> Some decl
  | None -> None

and parse_value_decl_statement env stmt =
  match stmt with
  | [ { datum = Token { kind = Ident name; _ }; span = name_span }; eq ]
    when token_kind Equals eq ->
      error
        ("missing value for binding: " ^ name ^ " at "
        ^ Format.asprintf "%a" Source_span.pp name_span)
  | { datum = Token { kind = KwRec; _ }; _ } :: rest ->
      parse_value_decl_after_prefix env ~recursive:true rest
  | rest -> parse_value_decl_after_prefix env ~recursive:false rest

and parse_value_decl_after_prefix env ~recursive stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwFn; _ }; _ }
    :: ({ datum = Token { kind = Ident name; _ }; span = name_span } as name_term)
    :: rest ->
      let _, value, rest = parse_fn env name_span rest in
      ensure_no_rest "function declaration" rest;
      Some
        {
          decl_name = id_of name_term name;
          decl_type = None;
          decl_value = value;
          decl_recursive = recursive;
        }
  | name_term :: rest when Option.is_some (binding_name_term name_term) -> (
      let name_id = Option.get (binding_name_term name_term) in
      match split_at_token Equals rest with
      | Some (before_eq, _, value_terms) ->
          if drop_separators value_terms = [] then
            error ("missing value for binding: " ^ name_id.name);
          let decl_type =
            match drop_separators before_eq with
            | [] -> None
            | colon :: typ_terms when token_kind Colon colon ->
                Some
                  (try parse_type_terms env typ_terms with
                  | Unsupported msg ->
                      unsupported ("binding " ^ name_id.name ^ " type: " ^ msg)
                  | Error msg ->
                      error ("binding " ^ name_id.name ^ " type: " ^ msg))
            | _ ->
                error
                  "binding parameters are not supported; use fn name(params) \
                   syntax"
          in
          let decl_value =
            try parse_all (fun ts -> parse_expr_prec env Top ts) value_terms with
            | Unsupported msg ->
                unsupported ("binding " ^ name_id.name ^ ": " ^ msg)
            | Error msg -> error ("binding " ^ name_id.name ^ ": " ^ msg)
          in
          let decl_value = decl_value in
          Some
            {
              decl_name = name_id;
              decl_type;
              decl_value;
              decl_recursive = recursive;
            }
      | None -> None)
  | _ -> None

and parse_effect_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwEffect; _ }; _ }
    :: ({ datum = Token { kind = Ident name; _ }; span = name_span } as name_term)
    :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, op_terms) ->
          let params = parse_decl_type_params "effect" name_span param_terms in
          Some
            (Syntax.EffectBinding
               {
                 name = id_of name_term name;
                 params;
                 ops = parse_effect_ops env op_terms;
                 public;
               })
      | None -> error "effect binding requires =")
  | _ -> None

and parse_trait_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwTrait; _ }; _ }
    :: ({ datum = Token { kind = Ident name; _ }; span = name_span } as name_term)
    :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, field_terms) ->
          let params =
            match parse_decl_type_params "trait" name_span param_terms with
            | [ param ] -> [ param ]
            | [] -> error "trait declaration requires exactly one parameter"
            | _ -> error "trait declaration accepts exactly one parameter"
          in
          let fields = parse_trait_fields env field_terms in
          Some
            (Syntax.TraitBinding
               { name = id_of name_term name; params; fields; public })
      | None -> error "trait binding requires =")
  | _ -> None

and parse_impl_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwImpl; _ }; _ } :: rest -> (
      match split_at_token Equals rest with
      | Some (trait_terms, _, module_kw :: module_rest)
        when token_kind KwModule module_kw ->
          (* [impl NAME : Trait(Args) = …] names the impl, making it reachable
             as a member so a use site can say which impl it means instead of
             having to [open] the defining module. The name is optional; without
             it the impl stays anonymous and arrives only through [open].
             See docs/wayfinder/topics/impl-visibility.md. *)
          let name, trait_terms =
            match split_at_token Colon trait_terms with
            | Some (name_terms, _, after) -> (
                match drop_separators name_terms with
                | [ ({ datum = Token { kind = Ident n; _ }; _ } as term) ] ->
                    (Some (id_of term n), after)
                | _ -> error "impl name must be a single identifier")
            | None -> (None, trait_terms)
          in
          let trait, args = parse_impl_trait env trait_terms in
          let field_terms, after_module, _ = brace_body "module" module_rest in
          ensure_no_rest "impl binding" after_module;
          let fields =
            split_statements field_terms
            |> List.map (fun field ->
                match parse_value_decl_statement env field with
                | Some { decl_name; decl_value; _ } ->
                    (decl_name.name, decl_value)
                | None -> error "expected impl let field")
          in
          Some
            (Syntax.ImplBinding { name; trait; args; fields; public })
      | Some _ | None -> error "impl binding requires = module { … }")
  | _ -> None

(* [Trait(Arg)] after [impl]: the trait's path and its one argument. *)
and parse_impl_trait env trait_terms =
  let trait, arg_terms = path_from_terms trait_terms in
  let args =
    match drop_separators arg_terms with
    | [ { datum = Group (Raw_syntax.Paren, items, _); _ } ] -> (
        let items = drop_separators items in
        if items = [] then error "impl argument list cannot be empty";
        match List.map (parse_type_terms env) (split_commas items) with
        | [ arg ] -> [ arg ]
        | _ -> error "impl declaration accepts exactly one trait argument")
    | [] -> error "impl declaration requires a parenthesized trait argument"
    | _ -> error "impl trait argument must be written as (Type)"
  in
  (trait, args)

and parse_open_statement env stmt =
  match drop_separators stmt with
  | open_kw :: rest when token_kind KwOpen open_kw ->
      let value, rest = parse_expr_prec env Top rest in
      ensure_no_rest "open statement" rest;
      Some value
  | _ -> None

and parse_import_statement env stmt =
  let spec = Parse_spec.punct KwImport in
  match Parse_spec.to_option spec env stmt with
  | Some () -> Some (parse_all (fun ts -> parse_expr_prec env Top ts) stmt)
  | None -> None

and parse_operator_value env start_span terms =
  let terms = drop_separators terms in
  let explicit_params, rest =
    match terms with
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        (parse_param_group env Explicitness.Explicit items, rest)
    | _ -> ([], terms)
  in
  let params = explicit_params in
  let body, rest, span = parse_body env "operator parameters" rest in
  let span = span_between start_span span in
  ( List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body,
    rest )

(* A rule's replacement, read as quoted syntax where the rule is written (M10):
   an expression, or for a [: Decl] form a brace group of declarations. *)
and parse_replacement env kind holes terms =
  let env = eager_env ~holes env in
  match kind, drop_separators terms with
  | Syntax.MacroAnnotation.Decl, [ { datum = Group (Raw_syntax.Brace, body, _); _ } ] ->
      if List.exists (fun stmt -> Option.is_some (parse_struct_field env stmt)) (split_statements body) then
        error "struct field declarations are deferred in declaration syntax templates";
      Syntax.ReplaceDecls (parse_module_bindings env body)
  | Syntax.MacroAnnotation.Decl, _ -> error "a Decl syntax form's replacement is written { declarations }"
  | Syntax.MacroAnnotation.Expr, _ -> Syntax.ReplaceExpr (parse_all (fun ts -> parse_expr_prec env Top ts) terms)

(* Reading quoted syntax, a declared role is registered as it is read, for the
   statements after it; otherwise expansion registers it (M7). *)
and declare_role ?macro_value env (name : Syntax.id) (role : Syntax.role) =
  if env.registers then begin
    Binding.extend env.operators ~name:name.name ~scope:name.scope ~kind:Binding.Role ~resolved_name:name.name ~role;
    env.declared <- env.declared + 1
  end;
  { role_name = name; role; macro_value }

and parse_operator_template_decl env (sym_id : Syntax.id) order value_terms =
  let sym = sym_id.name and sym_span = sym_id.span in
  let holes, rest =
    match drop_separators value_terms with
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        let holes =
          split_commas (drop_separators items)
          |> List.map (fun ts ->
              match drop_separators ts with
              | [ { datum = Token { kind = Operator "$"; _ }; _ }; { datum = Token { kind = Ident name; _ }; _ } ] -> name
              | _ -> error "operator template params must be $hole names")
        in
        (holes, rest)
    | _ -> error "operator template requires parameter list"
  in
  (* The replacement is the brace group itself, so it parses as a block. *)
  let group, span =
    match drop_separators rest with
    | [ ({ datum = Group (Raw_syntax.Brace, _, span); _ } as group) ] -> (group, span_between sym_span span)
    | _ -> error "expected { body } after operator template parameters"
  in
  let pattern =
    let hole name = Syntax.PartHole { hole = name; hole_kind = Syntax.HoleExpr; hole_span = sym_span } in
    let op_literal = Raw_syntax.syntax_token (Operator sym) sym_span in
    match holes with
    | [ lhs; rhs ] -> [ hole lhs; Syntax.PartToken op_literal; hole rhs ]
    | _ -> error "operator template must have 2 holes"
  in
  let replacement = Enforest_template.rewrite_holes [ group ] in
  List.iter
    (fun name -> if not (List.mem name (holes @ env.holes)) then error ("unbound syntax template hole in replacement: " ^ name))
    (Enforest_template.replacement_holes replacement);
  let rule = { Syntax.pattern; replacement = parse_replacement env Syntax.MacroAnnotation.Expr holes replacement; rule_span = span } in
  declare_role env sym_id
    (Binding.role ~declared_at:sym_span ~fixity:Syntax.InfixOp ?order
       (Syntax.Rules { rules_kind = Syntax.MacroAnnotation.Expr; rules = [ rule ] }))

(* An order group named where an operator, form or group declaration names it:
   a bare name resolves by scope set, like any binder; [M.g] reads [g] among the
   roles the unit [M] denotes exports. *)
and resolve_order env terms =
  let not_a_group () = error "an order group is named by an identifier or a dotted path M.g" in
  match drop_separators terms with
  | [ ({ datum = Token { kind = Ident name; _ }; _ } as term) ] -> (
      match Binding.find_order env.operators ~scope:(token_scope term) name with
      | Some order -> order
      | None -> error ("unknown order group: " ^ name))
  | ({ datum = Token { kind = Ident head; _ }; _ } as head_term) :: path -> (
      let rec members acc = function
        | dot :: { datum = Token { kind = Ident m; _ }; _ } :: rest when token_kind Dot dot -> members (m :: acc) rest
        | [] -> acc
        | _ -> not_a_group ()
      in
      match members [] path with
      | [] -> not_a_group ()
      | name :: rev_prefix ->
          let prefix = List.rev rev_prefix in
          let unit_expr =
            List.fold_left
              (fun m field -> stx ~span:head_term.span (Syntax.FieldAccess (m, field)))
              (var_of head_term head) prefix
          in
          let group_of (n, (r : Syntax.role)) =
            if String.equal n name && r.meaning = Syntax.OrderGroup then r.order else None
          in
          match List.find_map group_of (env.unit_roles unit_expr) with
          | Some order -> order
          | None -> error ("unknown order group: " ^ String.concat "." ((head :: prefix) @ [ name ])))
  | _ -> not_a_group ()

(* The terms naming one group at the front of [terms] - [g] or [M.N.g] - and the
   terms after them. *)
and take_order_ref terms =
  let rec go acc = function
    | dot :: ({ datum = Token { kind = Ident _; _ }; _ } as t) :: rest when token_kind Dot dot -> go (t :: dot :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  match drop_separators terms with
  | ({ datum = Token { kind = Ident _; _ }; _ } as t) :: rest -> go [ t ] rest
  | rest -> ([], rest)

(* [order name : stronger_than(g, …), weaker_than(g, …), weakest, assoc(left|right|none)]:
   precedence is relative, and a group is related only by declarations. The
   order is transitive; a declaration that would make it cyclic is an error. *)
and parse_order_decl env name_term name clauses =
  let groups items =
    List.map
      (fun ts ->
        match take_order_ref ts with
        | (_ :: _ as ref_terms), [] -> resolve_order env ref_terms
        | _ -> error "expected an order group")
      (split_commas items)
  in
  (* Clauses follow each other: a [,] would end the declaration's statement. *)
  let rec read_clauses (s, w, a, k) = function
    | [] -> (s, w, a, k)
    | { datum = Token { kind = Ident "weakest"; _ }; _ } :: rest -> read_clauses (s, w, a, true) rest
    | { datum = Token { kind = Ident "stronger_than"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        read_clauses (s @ groups items, w, a, k) rest
    | { datum = Token { kind = Ident "weaker_than"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        read_clauses (s, w @ groups items, a, k) rest
    | { datum = Token { kind = Ident "assoc"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest -> (
        match drop_separators items with
        | [ { datum = Token { kind = Ident "left"; _ }; _ } ] -> read_clauses (s, w, Syntax.LeftAssoc, k) rest
        | [ { datum = Token { kind = Ident "right"; _ }; _ } ] -> read_clauses (s, w, Syntax.RightAssoc, k) rest
        | [ { datum = Token { kind = Ident "none"; _ }; _ } ] -> read_clauses (s, w, Syntax.NonAssoc, k) rest
        | _ -> error "assoc is written assoc(left), assoc(right) or assoc(none)")
    | _ -> error "an order clause is stronger_than(…), weaker_than(…), weakest or assoc(left|right|none)"
  in
  let stronger_than, weaker_than, group_assoc, weakest = read_clauses ([], [], Syntax.LeftAssoc, false) (drop_separators clauses) in
  List.iter
    (fun (st : Syntax.order) ->
      List.iter
        (fun (wk : Syntax.order) ->
          match Syntax.order_relation st wk with
          | Syntax.Same | Stronger ->
              error
                (Printf.sprintf "order %s would be both stronger than %s and weaker than %s: the order would be cyclic"
                   name st.group_name wk.group_name)
          | Weaker | Unrelated -> ())
        weaker_than)
    stronger_than;
  let order = { Syntax.group = fresh_order_group name; group_name = name; group_assoc; weakest; stronger_than; weaker_than } in
  declare_role env (id_of name_term name)
    (Binding.role ~declared_at:name_term.span ~fixity:Syntax.PrefixOp ~order Syntax.OrderGroup)

and parse_syntax_template_decl env (head_id : Syntax.id) kind order body_terms rest =
  ensure_no_rest "syntax declaration" rest;
  let rules =
    Enforest_template.parse_rules ~available:env.holes ~head:head_id.name
      ~parse_replacement:(fun holes terms -> parse_replacement env kind holes terms)
      body_terms
  in
  Enforest_template.check_token_holes ~kind rules;
  declare_role env head_id
    (Binding.role ~declared_at:head_id.span ~fixity:Syntax.PrefixOp ?order
       (Syntax.Rules { rules_kind = kind; rules }))

and template_callbacks env trailing =
  { Enforest_template.parse_expr = (fun terms -> parse_all (fun ts -> parse_expr_prec env Top ts) terms);
    parse_expr_prefix = parse_expr_prec env;
    trailing;
    parse_pat_prefix = Enforest_pat.parse_pat_prefix;
    eager = env.eager }

(* A syntax form used where a declaration goes; [public] when written after
   [pub], which makes every declaration it returns public. *)
and parse_decl_template_use ?(public = false) env stmt =
  match drop_separators stmt with
  | ({ datum = Token { kind = Ident head; _ }; _ } as head_term) :: _ -> (
      match Binding.find_role env.operators ~fixity:Syntax.PrefixOp ~scope:(token_scope head_term) head with
      | Some ({ meaning = Syntax.Rules { rules_kind; rules }; from_unit; _ } as role) ->
          let inst, rest =
            Enforest_template.instantiate (template_callbacks env (Operand (head, role))) ~form:(id_of head_term head) ~kind:rules_kind
              ~position:Syntax.MacroAnnotation.Decl ~from_unit rules stmt
          in
          ensure_no_rest "declaration syntax template use" rest;
          Some (Syntax.InstantiateBinding { inst; public })
      | _ -> None)
  | _ -> None

(* The declared operator's name, an id spanning its parenthesised symbol. A
   hole there names it with a capture (M7 decision 6). *)
and operator_symbol kind sym_items sym_span =
  match drop_separators sym_items with
  | [ term ] when token_text term = Some "=>" -> error "=> is reserved and cannot be declared as an operator"
  | [ term ] when Option.is_some (token_text term) ->
      Syntax.fresh_id ~span:sym_span ~scope:(token_scope term) (Option.get (token_text term))
  | _ -> error (kind ^ " requires a symbol in parens")

(* The group an operator or form joins, written after its name: [None] when it
   joins none. A number there is the removed numeric precedence. *)
and parse_joined_order env terms =
  match drop_separators terms with
  | { datum = Token { kind = Int _; _ }; _ } :: _ ->
      error "numeric precedence was removed; declare an order group (order g : stronger_than(…)) and write infix (op) g"
  | terms -> (
      match take_order_ref terms with
      | [], rest -> (None, rest)
      | ref_terms, rest -> (Some (resolve_order env ref_terms), rest))

and parse_operator_shape env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident "infix"; _ }; span = infix_span }
    :: { datum = Group (Raw_syntax.Paren, sym_items, sym_span); _ }
    :: value_terms ->
      let order, value_terms = parse_joined_order env value_terms in
      Some (`Infix (operator_symbol "infix" sym_items sym_span, order, infix_span, value_terms))
  | { datum = Token { kind = Ident "prefix"; _ }; _ }
    :: { datum = Group (Raw_syntax.Paren, sym_items, sym_span); _ }
    :: value_terms ->
      let order, value_terms = parse_joined_order env value_terms in
      Some (`Prefix (operator_symbol "prefix" sym_items sym_span, order, value_terms))
  | _ -> None

and parse_operator_decl env stmt =
  match parse_operator_shape env stmt with
  | Some (`Prefix (name_id, order, value_terms)) -> (
      (* [prefix (op) g] is fixity only: [op x] calls the value [op]. *)
      match drop_separators value_terms with
      | [] ->
          Some (declare_role env name_id
                  (Binding.role ~declared_at:name_id.span ~fixity:Syntax.PrefixOp ?order Syntax.ApplyValue))
      | _ -> error "prefix operator with a body is not supported")
  | Some (`Infix (name_id, order, _, value_terms)) when drop_separators value_terms = [] ->
      (* [infix (op) g] is fixity only: [a op b] calls the value [op]. *)
      Some (declare_role env name_id
              (Binding.role ~declared_at:name_id.span ~fixity:Syntax.InfixOp ?order Syntax.ApplyValue))
  | Some (`Infix (name_id, order, infix_span, value_terms)) ->
      let is_template =
        match drop_separators value_terms with
        | { datum = Group (Raw_syntax.Paren, items, _); _ } :: _ -> (
            match drop_separators (List.concat (split_commas (drop_separators items))) with
            | { datum = Token { kind = Operator "$"; _ }; _ } :: _ -> true
            | _ -> false)
        | _ -> false
      in
      if is_template then Some (parse_operator_template_decl env name_id order value_terms)
      else begin
        let value, rest = parse_operator_value env infix_span value_terms in
        ensure_no_rest "infix declaration" rest;
        Some (declare_role ~macro_value:value env name_id
                (Binding.role ~declared_at:name_id.span ~fixity:Syntax.InfixOp ?order Syntax.CallMacro))
      end
  | None -> (
      match drop_separators stmt with
      | { datum = Token { kind = Ident "order"; _ }; _ }
        :: ({ datum = Token { kind = Ident name; _ }; _ } as name_term)
        :: after -> (
          match drop_separators after with
          | [] -> Some (parse_order_decl env name_term name [])
          | colon :: clauses when token_kind Colon colon -> Some (parse_order_decl env name_term name clauses)
          | _ -> error "an order group is declared order name or order name : clauses")
      | { datum = Token { kind = Ident "syntax"; _ }; _ } :: head_term :: after -> (
          let head =
            match head_term.datum with
            | Token { kind = Ident name; _ } -> id_of head_term name
            | _ -> error "syntax declaration head must be an identifier"
          in
          let kind, after = Enforest_template.syntax_kind after in
          let order, after = parse_joined_order env after in
          match after with
          | { datum = Group (Raw_syntax.Brace, body_terms, _); _ } :: rest ->
              Some (parse_syntax_template_decl env head kind order body_terms rest)
          | _ -> unsupported "unsupported syntax declaration shape")
      | { datum = Token { kind = Ident "syntax"; _ }; _ } :: _ -> unsupported "unsupported syntax declaration shape"
      | _ -> None)

and scoped_binding_to_expr env span stmt body =
  let public, stmt = parse_public_prefix stmt in
  if public then error "pub is not supported inside do blocks";
  (
      match parse_effect_binding env false stmt with
      | Some (Syntax.EffectBinding { name; params; ops; _ }) ->
          stx ~span (Syntax.EffectDef { name; params; ops; body })
      | Some _ -> error "unexpected non-effect binding"
      | None -> (
          match parse_trait_binding env false stmt with
          | Some (Syntax.TraitBinding { name; params; fields; _ }) ->
              stx ~span (Syntax.TraitDef { name; params; fields; body })
          | Some _ -> error "unexpected non-trait binding"
          | None -> (
              match parse_impl_binding env false stmt with
              | Some
                  (Syntax.ImplBinding
                     { name; trait; args; fields; _ }) ->
                  stx ~span
                    (Syntax.ImplDef
                       { name; trait; args; fields; body })
              | Some _ -> error "unexpected non-impl binding"
              | None ->
                  (* An expression statement: its value is discarded. *)
                  let value = parse_all (fun ts -> parse_expr_prec env Top ts) stmt in
                  stx ~span
                    (Syntax.Let { name = id ~span:value.span "_"; type_ = None; value; body; recursive = false }))))

(* A [{ … }] body read now, as quoted syntax. A trailing [;] discards the
   block's value: every item is a statement and the block is [()]. *)
and parse_do_body_terms env span body_terms =
  let discards = match List.rev body_terms with last :: _ -> is_separator last | [] -> false in
  let items =
    map_context_statements env
      (fun ~last stmt ->
        if last && not discards then Either.Right (parse_all (fun ts -> parse_expr_prec env Top ts) stmt)
        else Either.Left (do_statement env span stmt))
      body_terms
  in
  let rev_wrappers, body =
    match List.rev items with
    | [] -> error "empty block"
    | Either.Right body :: rev -> (rev, body)
    | rev -> (rev, unit ~span ())
  in
  List.fold_left
    (fun acc item -> match item with Either.Left wrap -> wrap acc | Either.Right _ -> acc)
    body rev_wrappers

(* One statement of a block, as the wrapper that scopes it over the rest. *)
and do_statement env span stmt =
  match parse_rec_group env stmt with
  | Some members -> fun acc -> stx ~span (Syntax.LetRecGroup { members; body = acc })
  | None ->
                match parse_operator_decl env stmt with
                | Some { role_name = name; role; macro_value } ->
                    fun acc ->
                      let body =
                        match macro_value with
                        | Some value -> stx ~span (Syntax.MacroDef { name; value; body = acc; kind = None; output = None })
                        | None -> acc
                      in
                      stx ~span (Syntax.SyntaxDef { name; role; body })
                | None -> (
                    match parse_macro_binding env false stmt with
                    | Some
                        (Syntax.MacroBinding
                           { name; value; public = false; kind; output }) ->
                        fun acc ->
                          stx ~span
                            (Syntax.MacroDef { name; value; body = acc; kind; output })
                    | Some (Syntax.MacroBinding { public = true; _ }) ->
                        error "pub macro is not supported inside do blocks"
                    | Some _ -> error "unexpected non-macro binding"
                    | None -> (
                        match parse_binding_statement env stmt with
                        | Some
                            {
                              decl_name = name;
                              decl_type = type_;
                              decl_value = value;
                              decl_recursive = recursive;
                            } ->
                            fun acc ->
                              stx ~span
                                (Syntax.Let
                                   { name; type_; value; body = acc; recursive })
                        | None -> (
                            match parse_open_statement env stmt with
                            | Some name ->
                                fun acc -> stx ~span (Syntax.Open (name, acc, ""))
                            | None -> (
                                match parse_import_statement env stmt with
                                | Some value ->
                                    fun acc ->
                                      stx ~span
                                        (Syntax.Let
                                           {
                                             name = id ~span:value.span "_";
                                             type_ = None;
                                             value;
                                             body = acc;
                                             recursive = false;
                                           })
                                | None ->
                                    fun acc ->
                                      scoped_binding_to_expr env span stmt acc))
                        ))

and parse_public_prefix stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwPub; _ }; _ } :: rest -> (true, rest)
  | rest -> (false, rest)

and parse_macro_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwMacro; _ }; _ }
    :: ({ datum = Token { kind = Ident name; _ }; span = name_span } as name_term)
    :: rest ->
      let (kind, output), value, rest =
        parse_fn ~kind_annotation:true env name_span rest
      in
      ensure_no_rest "macro binding" rest;
      Some
        (Syntax.MacroBinding
           { name = id_of name_term name; value; public; kind; output })
  | _ -> None

and parse_pattern_syn_binding _env public stmt =
  let header =
  Parse_spec.seq3
    (Parse_spec.punct KwPattern)
    Parse_spec.str_ident
    (Parse_spec.opt (Parse_spec.custom_spec ~name:"params_group" (fun _env -> function
      | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
          Some (items, rest)
      | _ -> None)))
in
match Parse_spec.parse header _env stmt with
| Some (((), (name, name_term), param_terms), rest) -> (
    match Enforest_util.drop_separators rest with
    | equals :: rhs_terms when token_kind Equals equals ->
        let params = match param_terms with
          | Some terms ->
              split_commas (Enforest_util.drop_separators terms)
              |> List.map (fun ts ->
                  match Enforest_util.drop_separators ts with
                  | [ ({ datum = Token { kind = Ident p; _ }; _ } as term) ] ->
                      id_of term p
                  | _ -> error "expected pattern synonym parameter name")
          | None -> []
        in
          let rhs = Enforest_pat.parse_pat_terms rhs_terms in
          Some
            (Syntax.PatternSynBinding
               { name = id_of name_term name; params; rhs; public })
      | _ -> error "expected '=' after pattern synonym parameters")
  | None -> None

and parse_macro_call_binding env public stmt =
  (* A bare application statement [f(args)] at declaration position is a
     (decl-)macro invocation: the head is resolved to a macro by the expander,
     which reclassifies it into the internal [MacroCallBinding]. There is no
     [@] marker; the same [f(args)] surface is used as for function calls. *)
  let args_spec =
    Parse_spec.custom_spec ~name:"args" (fun _env items -> Some (items, []))
  in
  Parse_spec.to_option
    (Parse_spec.map
       (Parse_spec.seq3 Parse_spec.str_ident
          (Parse_spec.paren_group args_spec)
          Parse_spec.eof)
       (fun ((name, name_term), (items, _), ()) ->
         let f = var_of name_term name in
         let args =
           match macro_call_args env f items, Enforest_util.drop_separators items with
           | Some args, _ -> args
           | None, [] -> [ Syntax.CapExpr (unit ~span:Source_span.synthetic ()) ]
           | None, _ -> List.map (fun a -> Syntax.CapExpr a) (parse_args env items)
         in
         Syntax.MacroCallBinding { f; args; public }))
    env stmt

(* [rec A = … and B = …]: a recursive group, its members in order. A lone [rec]
   binding is not a group. [and] is contextual, as in a type chain. *)
and parse_rec_group env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwRec; _ }; _ } :: rest -> (
      match split_and_chain rest with
      | [] | [ _ ] -> None
      | segments ->
          let members =
            List.map
              (fun segment ->
                match parse_value_decl_after_prefix env ~recursive:true segment with
                | Some { decl_name; decl_type = Some typ; decl_value; _ } ->
                    (decl_name, stx ~span:(syntax_span segment) (Syntax.Annotated { inner = decl_value; typ }))
                | Some { decl_name; decl_type = None; decl_value; _ } -> (decl_name, decl_value)
                | None -> error "expected name = value in a rec … and … group")
              segments
          in
          let names = List.map (fun ((n : Syntax.id), _) -> n.name) members in
          (match List.find_opt (fun n -> List.length (List.filter (String.equal n) names) > 1) names with
           | Some dup -> error ("duplicate name in a rec … and … group: " ^ dup)
           | None -> ());
          Some members)
  | _ -> None

and parse_value_binding env public stmt =
  match parse_rec_group env stmt with
  | Some members -> Some (Syntax.RecGroupBinding { members; public })
  | None ->
  match parse_value_decl_statement env stmt with
  | Some { decl_name = name; decl_type; decl_value; decl_recursive } ->
      let value =
        match decl_type with
        | Some typ ->
            stx ~span:(syntax_span stmt)
              (Syntax.Annotated { inner = decl_value; typ })
        | None -> decl_value
      in
      Some
        (Syntax.LetBinding { name; value; public; recursive = decl_recursive })
  | None -> None

(* [open <module-expr>] as a module/struct item. The operator side of the open is
   already handled by parsing the module expression: an [import "path"] inside it
   goes through [Enforest_forms.parse_import], which harvests that module's syntax
   exports into [env] — and because statements are parsed in source order, the
   harvest scopes over exactly the items that follow, matching the elaborator's
   treatment of [OpenBinding]. *)
and parse_open_binding env public stmt =
  match parse_open_statement env stmt with
  | Some _ when public -> error "open is not a public item"
  | Some mod_expr -> Some (Syntax.OpenBinding (mod_expr, ""))
  | None -> None

(* [export M] or [export M.{a, b}]: the selection is the trailing [.{ … }]. *)
and parse_export_binding env public stmt =
  match drop_separators stmt with
  | kw :: rest when token_kind KwExport kw ->
      if public then error "export is not a public item: an export already publishes";
      let module_terms, names =
        match List.rev (drop_separators rest) with
        | { datum = Group (Raw_syntax.Brace, items, _); _ } :: dot :: rev_module when token_kind Dot dot ->
            let name item =
              match drop_separators item with
              | [ { datum = Token { kind = Ident n; _ }; _ } ] -> n
              | _ -> error "export M.{a, b} names members"
            in
            (List.rev rev_module, Some (List.map name (List.filter (fun ts -> drop_separators ts <> []) (split_commas (drop_separators items)))))
        | _ -> (rest, None)
      in
      let m, rest = parse_expr_prec env Top module_terms in
      ensure_no_rest "export" rest;
      Some (Syntax.ExportBinding { m; names; public = true })
  | _ -> None

(* A role declaration as bindings: the role, then the macro its body defines. *)
and role_bindings public { role_name = name; role; macro_value } =
  Syntax.SyntaxBinding { name; role; public }
  :: (match macro_value with Some value -> [ Syntax.MacroBinding { name; value; public; kind = None; output = None } ] | None -> [])

and parse_module_binding env stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_operator_decl env stmt with
  | Some decl -> role_bindings public decl
  | None -> (
      match
        first_some
          [
            parse_open_binding env public;
            parse_export_binding env public;
            parse_macro_binding env public;
            parse_macro_call_binding env public;
            parse_pattern_syn_binding env public;
            parse_effect_binding env public;
            parse_trait_binding env public;
            parse_impl_binding env public;
            parse_value_binding env public;
          ]
          stmt
      with
      | Some binding -> [ binding ]
      | None ->
          let desc =
            match Enforest_util.drop_separators stmt with
            | t :: _ -> Enforest_util.desc_token t
            | [] -> "(empty)"
          in
          unsupported (Printf.sprintf "unsupported module item: %s" desc))

and parse_module_statement env stmt =
  match drop_separators stmt with
  (* A lone [$d] is a declaration hole: only [quote]'s rewriting spells an id
     with [$]. *)
  | [ ({ datum = Token { kind = Ident name; _ }; _ } as term) ] when String.length name > 1 && name.[0] = '$' ->
      [ Syntax.HoleBinding (id_of term name) ]
  | [] -> []
  | _ -> (
      let public, unprefixed = parse_public_prefix stmt in
      match parse_decl_template_use ~public env unprefixed with
      | Some binding -> [ binding ]
      | None -> parse_module_binding env stmt)

and parse_module_bindings env body_terms =
  map_context_statements env (fun ~last:_ stmt -> parse_module_statement env stmt) body_terms |> List.concat

and parse_struct_field env stmt =
  match drop_separators stmt with
  | [ { datum = Token { kind = Ident name; _ }; _ }; colon ]
    when token_kind Colon colon ->
      error ("missing type for struct field: " ^ name)
  | { datum = Token { kind = Ident name; _ }; _ } :: colon :: typ_terms
    when token_kind Colon colon ->
      if List.exists (token_kind Equals) typ_terms then None
      else Some (Syntax.FieldBinding { name; type_ = parse_type_terms env typ_terms })
  | _ -> None

and parse_struct_binding env stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_operator_decl env stmt with
  | Some decl ->
      if public then error "pub syntax is not supported inside structs"
      else Some (role_bindings false decl)
  | None -> (
      match parse_macro_binding env public stmt with
      | Some _ when public -> error "pub macro is not supported inside structs"
      | Some binding -> Some [ binding ]
      | None -> (
          match parse_macro_call_binding env public stmt with
          | Some _ when public ->
              error "pub macro call is not supported inside structs"
          | Some binding -> Some [ binding ]
          | None -> (
              match
                first_some
                  [
                    parse_open_binding env public;
                    parse_method_binding env public;
                    parse_effect_binding env public;
                    parse_trait_binding env public;
                    parse_impl_binding env public;
                    parse_value_binding env public;
                  ]
                  stmt
              with
              | Some binding -> Some [ binding ]
              | None ->
                  let desc =
                    match Enforest_util.drop_separators stmt with
                    | t :: _ -> Enforest_util.desc_token t
                    | [] -> "(empty)"
                  in
                  unsupported
                    (Printf.sprintf "unsupported struct item: %s" desc))))

and parse_struct_statement env stmt =
  match parse_decl_template_use env stmt with
  | Some binding -> [ binding ]
  | None -> Option.value ~default:[] (parse_struct_binding env stmt)

and parse_struct_items env body_terms =
  let env = registering_env env in
  map_context_statements env
    (fun ~last:_ stmt ->
      match parse_struct_field env stmt with
      | Some field -> [ field ]
      | None -> parse_struct_statement env stmt)
    body_terms
  |> List.concat

let parse_terms env terms = parse_all (fun ts -> parse_expr_prec env Top ts) terms

(* The first statement of a [{ … }] body as a form scoping over the rest, which
   stays unread until expansion reaches it (M9). A trailing [;] discards the
   body's value. *)
let parse_block_head env span terms =
  match take_statement terms with
  | [], _ -> error "empty block"
  | stmt, [] -> parse_all (fun ts -> parse_expr_prec env Top ts) stmt
  | stmt, rest -> (
      match drop_separators rest with
      | [] -> do_statement env span stmt (unit ~span ())
      | more -> do_statement env span stmt (stx ~span:(syntax_span more) (Syntax.Block more)))

(* A block statement that uses a [: Decl] syntax form: its declarations bind for
   the rest of the block, which expansion reads after filling them. The block's
   last item is its value, an expression position. *)
let parse_block_decl_form env terms =
  let stmt, rest = take_statement terms in
  if rest = [] then None else
  match drop_separators stmt with
  | ({ datum = Token { kind = Ident head; _ }; _ } as head_term) :: _ -> (
      match Binding.find_role env.operators ~fixity:Syntax.PrefixOp ~scope:(token_scope head_term) head with
      | Some { meaning = Syntax.Rules { rules_kind = Syntax.MacroAnnotation.Decl; _ }; _ } -> (
          match parse_decl_template_use env stmt with
          | Some (Syntax.InstantiateBinding { inst; _ }) -> Some (inst, rest)
          | _ -> None)
      | _ -> None)
  | _ -> None

let std_import_stx () = stx (Syntax.Import { path = Compiler_names.Module_name.std_import_path; scope = Scope_set.empty })

(* A source read as an expression: one body, read as expansion reaches it.
   [?open_prelude] is the expression entry point's implicit leading
   [open (import "std")]: a bare expression has nowhere to write the open. *)
let parse_expr ?file ?(open_prelude = false) source =
  try
    let terms = Raw_syntax.read ?file source in
    let body = stx ~span:(syntax_span terms) (Syntax.Block terms) in
    if open_prelude then stx (Syntax.Open (std_import_stx (), body, "")) else body
  with Raw_syntax.Error msg -> error msg

let parse_type ?file source =
  let env = lazy_env (Binding.create ()) in
  try Raw_syntax.read ?file source |> parse_all (parse_type_entry env)
  with Raw_syntax.Error msg -> error msg

let parse_pat ?file source =
  try Raw_syntax.read ?file source |> parse_pat_terms
  with Raw_syntax.Error msg -> error msg

(* A unit: its items, read one form at a time as expansion reaches them. Units
   are strict: a unit that wants the prelude writes [open (import "std")]. *)
let parse_module ?file source =
  try stx (Syntax.Module { bindings = [ Syntax.Items (Raw_syntax.read ?file source) ] })
  with Raw_syntax.Error msg -> error msg
