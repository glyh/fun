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
          parse_all (fun ts -> parse_expr_prec env 0 ts) part)
        parts

and parse_group_expr env delimiter items span =
  match delimiter with
  | Raw_syntax.Paren -> (
      let items = drop_separators items in
      match items with
      | [] -> unit ~span ()
      | [ { datum = Token { kind = Operator name; _ }; _ } ] -> var ~span name
      | _ -> (
          match split_at_token Colon items with
          | Some (expr_terms, _, typ_terms) ->
              stx ~span
                (Syntax.Annotated
                   {
                     inner =
                       parse_all (fun ts -> parse_expr_prec env 0 ts) expr_terms;
                     typ = parse_type_terms env typ_terms;
                   })
          | None -> (
              match split_commas items with
              | [ only ] ->
                  {
                    (parse_all (fun ts -> parse_expr_prec env 0 ts) only) with
                    span;
                  }
              | parts ->
                  let exprs =
                    List.map
                      (parse_all (fun ts -> parse_expr_prec env 0 ts))
                      parts
                  in
                  stx ~span (Syntax.Prod exprs))))
  | Bracket -> unsupported "bare bracket expression is not in Phase 7A"
  | Brace -> unsupported "bare brace expression is not in Phase 7A"

and parse_record_expr_fields env items =
  split_statements items
  |> List.map (fun part ->
      match drop_separators part with
      | { datum = Token { kind = Ident name; _ }; _ } :: eq :: value_terms
        when token_kind Equals eq ->
          (name, parse_all (fun ts -> parse_expr_prec env 0 ts) value_terms)
      | _ -> error "expected record field of the form name = expr")

and parse_type_terms env terms = parse_all (parse_type_entry env) terms

and parse_type_entry env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  parse_type_arrow env terms

and parse_type_arrow env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_plus env terms in
  match drop_separators rest with
  | { datum = Token { kind = ThinArrow; _ }; _ } :: rest ->
      let cod, rest = parse_type_arrow env (drop_separators rest) in
      let lhs =
        stx
          ~span:(span_between lhs.span cod.span)
          (Syntax.Arrow (Explicitness.Explicit, None, lhs, None, cod))
      in
      parse_type_arrow_effect env lhs rest
  | rest -> parse_type_arrow_effect env lhs rest

and parse_type_arrow_effect env (lhs : Syntax.t) (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = KwCan; _ }; _ } :: rest -> (
      match lhs.kind with
      | Syntax.Arrow _ ->
          let eff, rest = parse_can_effect_row env rest in
          parse_type_arrow_effect env (attach_effects lhs eff) rest
      | _ -> (lhs, terms))
  | terms -> (lhs, terms)

and parse_type_plus env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_product env terms in
  parse_type_plus_tail env lhs rest

and parse_type_plus_tail env (lhs : Syntax.t) (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = Operator "+"; _ }; span = op_span } :: rest ->
      let rhs, rest = parse_type_product env rest in
      let span = span_between lhs.span rhs.span in
      let lhs =
        ap ~span
          (ap ~span (var ~span:op_span "+") Explicitness.Explicit lhs)
          Explicitness.Explicit rhs
      in
      parse_type_plus_tail env lhs rest
  | rest -> (lhs, rest)

and parse_type_product env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_atom env terms in
  let lhs, rest = parse_type_postfix env lhs rest in
  parse_type_product_tail env lhs rest

and parse_type_product_tail env (lhs : Syntax.t) (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = Operator "*"; _ }; _ } :: rest ->
      let rhs, rest = parse_type_atom env rest in
      let rhs, rest = parse_type_postfix env rhs rest in
      let elems =
        (match lhs.kind with Syntax.ProdTy xs -> xs | _ -> [ lhs ])
        @ match rhs.kind with Syntax.ProdTy xs -> xs | _ -> [ rhs ]
      in
      parse_type_product_tail env
        (stx ~span:(span_between lhs.span rhs.span) (Syntax.ProdTy elems))
        rest
  | rest -> (lhs, rest)

and parse_type_atom env (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | [] -> error "expected type"
  | { datum = Token { kind = Ident name; _ }; span } :: rest ->
      (var ~span name, rest)
  | { datum = Token { kind = KwUnit; _ }; span } :: rest ->
      (var ~span "Unit", rest)
  | { datum = Token { kind = KwSelfType; _ }; span } :: rest ->
      (stx ~span Syntax.SelfType, rest)
  | { datum = Token { kind = KwFn; _ }; span } :: rest ->
      let _, expr, rest = parse_fn env span rest in (expr, rest)
  | { datum = Token { kind = KwModule; _ }; span } :: rest ->
      parse_module_expr env span rest
  | { datum = Token { kind = KwSig; _ }; span } :: rest ->
      parse_sig_expr env span rest
  | { datum = Token { kind = KwStruct; _ }; span } :: rest ->
      parse_struct_expr env span rest
  | { datum = Token { kind; _ }; _ } :: _ ->
      let name = keyword_name kind in
      if Option.is_some name then
        let name = Option.get name in
        if
          String.equal name "ref"
          || String.equal name "EffectRow"
          || String.equal name "Type" || String.equal name "I64"
          || String.equal name "Bool" || String.equal name "Unit"
          || String.equal name "Char" || String.equal name "String"
          || String.equal name "Absurd"
        then
          match terms with
          | { span; _ } :: rest -> (var ~span name, rest)
          | _ -> error "expected type token"
        else unsupported ("unexpected keyword in type: " ^ name)
      else error "expected type atom"
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: arrow :: rest
    when token_kind ThinArrow arrow && List.exists (token_kind Colon) items ->
      let params = parse_param_group env Explicitness.Explicit items in
      let (cod : Syntax.t), rest = parse_type_arrow env rest in
      let (result : Syntax.t) =
        List.fold_right
          (fun (p : Syntax.param) (acc : Syntax.t) ->
            let dom = Option.value ~default:(unit_type ()) p.type_ in
            stx
              ~span:(span_between span acc.span)
              (Syntax.Arrow (Explicitness.Explicit, Some p.name, dom, None, acc)))
          params cod
      in
      (result, rest)
  | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
      let items = drop_separators items in
      let ty =
        match split_commas items with
        | [ only ] -> parse_all (parse_type_entry env) only
        | _ -> error "tuple/product types use * syntax, not comma syntax"
      in
      (ty, rest)
  | { datum = Group (Raw_syntax.Bracket, items, _); _ } :: rest ->
      let (params : Syntax.param list) =
        parse_param_group env Explicitness.Implicit items
      in
      let cod_items, rest =
        match drop_separators rest with
        | arrow :: cod_terms when token_kind ThinArrow arrow -> (cod_terms, [])
        | _ -> error "expected -> after implicit Pi parameters"
      in
      let cod = parse_all (parse_type_entry env) cod_items in
      let result =
        List.fold_right
          (fun (p : Syntax.param) acc ->
            let dom = Option.value ~default:(unit_type ()) p.type_ in
            stx
              ~span:(span_between p.name.span cod.span)
              (Syntax.Arrow (Explicitness.Implicit, Some p.name, dom, None, acc)))
          params cod
      in
      (result, rest)
  | { datum = Group (Raw_syntax.Brace, _, _); _ } :: _ ->
      unsupported
        "bare brace type syntax is only supported in effect rows after can"

and parse_type_postfix env (lhs : Syntax.t) (terms : Raw_syntax.t list) :
    Syntax.t * Raw_syntax.t list =
  match terms with
  | { datum = Group (Raw_syntax.Paren, items, _); span = group_span } :: rest ->
      let args = parse_args env items in
      let call_span = span_between lhs.span group_span in
      let lhs =
        List.fold_left
          (fun f arg -> ap ~span:call_span f Explicitness.Explicit arg)
          lhs args
      in
      parse_type_postfix env lhs rest
  | dot :: field :: rest when token_kind Dot dot -> (
      match token_text field with
      | Some name ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.FieldAccess (lhs, name)) in
          parse_type_postfix env lhs rest
      | None ->
          unsupported
            "numeric projection in type is handled by the compatibility parser")
  | _ -> (lhs, terms)

and parse_param_item env explicitness terms =
  let terms = drop_separators terms in
  match terms with
  | [] when explicitness = Explicitness.Explicit ->
      param ~type_:(unit_type ()) Explicitness.Explicit "_"
  | [ { datum = Token { kind = Ident name; _ }; span } ] ->
      param ~span explicitness name
  | { datum = Token { kind = Ident name; _ }; span } :: colon :: typ_terms
    when token_kind Colon colon ->
      param ~span ~type_:(parse_type_terms env typ_terms) explicitness name
  | _ -> error "expected parameter of the form name or name : Type"

and parse_param_group env explicitness items =
  let items = drop_separators items in
  match items with
  | [] when explicitness = Explicitness.Explicit ->
      [ param ~type_:(unit_type ()) Explicitness.Explicit "_" ]
  | [] -> error "empty implicit parameter list"
  | _ -> List.map (parse_param_item env explicitness) (split_commas items)

and parse_fn_parts env ?(allow_empty = false) ?(kind_annotation = false) start_span terms =
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
          | ({ datum = Group (Raw_syntax.Bracket, _, _); _ } as g) :: _ -> g.span
          | _ -> start_span in
        require_adjacent_span previous_span group.span "explicit fn parameter list";
        (parse_param_group env Explicitness.Explicit items, rest)
    | rest when implicit_params <> [] || allow_empty -> ([], rest)
    | _ -> error "fn requires at least one parameter list"
  in
  let params = implicit_params @ explicit_params in
  let kind, rest = if kind_annotation then
    match drop_separators rest with
    | { datum = Token { kind = Colon; _ }; _ } :: { datum = Token { kind = Ident k; _ }; _ } :: rest ->
      (Syntax.MacroKind.of_string k, drop_separators rest)
    | _ -> (None, rest) else (None, rest) in
  let body, rest, span =
    match drop_separators rest with
    | arrow :: body_terms when token_kind ThinArrow arrow ->
        let body = parse_all (fun ts -> parse_expr_prec env 0 ts) body_terms in
        (body, [], span_between start_span body.span)
    | do_kw :: body_rest when token_kind KwDo do_kw ->
        let body_terms, rest, span = collect_until_end do_kw.span body_rest in
        (parse_do_body_terms env span body_terms, rest, span_between start_span span)
    | _ -> error "expected -> or do after fn parameters"
  in
  (params, kind, body, rest, span)

and parse_fn ?(kind_annotation = false) env start_span terms =
  let params, kind, body, rest, span = parse_fn_parts ~kind_annotation env start_span terms in
  (kind, List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body, rest)

and parse_method_params env items =
  let items = drop_separators items in
  if items = [] then [] else parse_param_group env Explicitness.Explicit items

and parse_method_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwMethod; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: ({ datum = Group (Raw_syntax.Paren, items, _); _ } as params_group)
    :: rest ->
      require_adjacent_span name_span params_group.span "method parameter list";
      let params = parse_method_params env items in
      let body, rest =
        match drop_separators rest with
        | arrow :: body_terms when token_kind ThinArrow arrow ->
            (parse_all (fun ts -> parse_expr_prec env 0 ts) body_terms, [])
        | do_kw :: body_rest when token_kind KwDo do_kw ->
            let body_terms, rest, span =
              collect_until_end do_kw.span body_rest
            in
            (parse_do_body_terms env span body_terms, rest)
        | _ -> error "expected -> or do after method parameters"
      in
      ensure_no_rest "method declaration" rest;
      Some
        (Syntax.MethodBinding
           { name = id ~span:name_span name; params; body; public })
  | { datum = Token { kind = KwMethod; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; _ }
    :: _ ->
      error
        ("method declaration requires a parenthesized parameter list: " ^ name)
  | _ -> None

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
      (fun ts -> parse_all (fun ts -> parse_expr_prec env 0 ts) ts);
    parse_do_body_terms = parse_do_body_terms env;
    parse_pat_terms;
    is_expr_start = is_expr_start env;
  }

and parse_if env start_span terms =
  Enforest_forms.parse_if (form_callbacks env) start_span terms

and parse_match env start_span terms =
  Enforest_forms.parse_match (form_callbacks env) start_span terms

and parse_group_arg env items =
  Enforest_forms.parse_group_arg (form_callbacks env) items

and parse_effect_row_terms env terms =
  Enforest_forms.parse_effect_row_terms (form_callbacks env) terms

and parse_can_effect_row env terms =
  Enforest_forms.parse_can_effect_row (form_callbacks env) terms

and attach_effects lhs eff = Enforest_forms.attach_effects lhs eff

and parse_ref env start_span terms =
  Enforest_forms.parse_ref (form_callbacks env) start_span terms

and parse_deref env start_span terms =
  Enforest_forms.parse_deref (form_callbacks env) start_span terms

and parse_resume env start_span terms =
  Enforest_forms.parse_resume (form_callbacks env) start_span terms

and parse_perform env start_span terms =
  Enforest_forms.parse_perform (form_callbacks env) start_span terms

and parse_import env start_span terms =
  Enforest_forms.parse_import env start_span terms

and parse_module_expr env start_span terms =
  match drop_separators terms with
  | do_kw :: _ when token_kind KwDo do_kw ->
      error "module do syntax is not supported; use module ... end"
  | _ ->
      let body_terms, rest, span = collect_until_end start_span terms in
      let bindings = parse_module_bindings env body_terms in
      ( stx ~span:(span_between start_span span) (Syntax.Module { bindings }),
        rest )

and parse_sig_expr env start_span terms =
  let body_terms, rest, span = collect_until_end start_span terms in
  let bindings =
    split_statements body_terms
    |> List.map (fun stmt ->
        match drop_separators stmt with
        | { datum = Token { kind = Ident name; _ }; span = name_span }
          :: colon :: typ_terms
          when token_kind Colon colon ->
            Syntax.LetBinding
              {
                name = id ~span:name_span name;
                value = parse_type_terms env typ_terms;
                public = true;
                recursive = false;
              }
        | _ -> error "expected signature field name : type")
  in
  (stx ~span:(span_between start_span span) (Syntax.Module { bindings }), rest)

and parse_struct_expr env start_span terms =
  match drop_separators terms with
  | do_kw :: _ when token_kind KwDo do_kw ->
      error "struct do syntax is not supported; use struct ... end"
  | _ ->
      let body_terms, rest, span = collect_until_end start_span terms in
      let con_fields, bindings = parse_struct_items env body_terms in
      ( stx
          ~span:(span_between start_span span)
          (Syntax.Struct { con_fields; bindings }),
        rest )

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
      | Token { kind = KwTrue; _ } ->
          (atom ~span:term.span (Atom.Bool true), rest)
      | Token { kind = KwFalse; _ } ->
          (atom ~span:term.span (Atom.Bool false), rest)
      | Token { kind = KwUnit; _ } -> (var ~span:term.span "Unit", rest)
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
      | Token { kind = KwType; _ } ->
          error
            "type ... in syntax is not supported; use do blocks and type \
             declarations"
      | Token { kind = KwTrait; _ } ->
          error
            "trait ... in syntax is not supported; use do blocks and trait \
             declarations"
      | Token { kind = KwImpl; _ } ->
          error
            "impl ... in syntax is not supported; use do blocks and impl \
             declarations"
      | Token { kind = KwDo; _ } -> parse_do env term.span rest
      | Token { kind = KwFn; _ } ->
          let _, expr, rest = parse_fn env term.span rest in (expr, rest)
      | Token { kind = KwIf; _ } -> parse_if env term.span rest
      | Token { kind = KwMatch; _ } -> parse_match env term.span rest
      | Token { kind = KwRef; _ } -> parse_ref env term.span rest
      | Token { kind = KwDeref; _ } -> parse_deref env term.span rest
      | Token { kind = KwResume; _ } -> parse_resume env term.span rest
      | Token { kind = KwPerform; _ } -> parse_perform env term.span rest
      | Token { kind = KwImport; _ } -> parse_import env term.span rest
      | Token { kind = KwModule; _ } -> parse_module_expr env term.span rest
      | Token { kind = KwSig; _ } -> parse_sig_expr env term.span rest
      | Token { kind = KwStruct; _ } -> parse_struct_expr env term.span rest
      | Token { kind = Ident name; _ } -> (
          match
            Operator_env.find_prefix ~syntax_class:env.syntax_class
              env.operators name
          with
          | Some { Operator_env.expansion = Operator_env.Template template; _ }
            ->
              expand_syntax_template env term.span template (term :: rest)
          | Some op ->
              let rhs, rest = parse_expr_prec env op.precedence rest in
              let span = span_between term.span rhs.span in
              let f = var ~span:term.span name in
              let expr =
                match op.expansion with
                | Operator_env.Macro ->
                    stx ~span
                      (Syntax.MacroCall
                         ( f,
                           [ syntax_operator_arg ~span ~use_span:term.span op
                              [ rhs ] ] ))
                | Operator_env.Template _ ->
                    error
                      "internal error: template syntax should expand before \
                       operand parsing"
                | _ -> ap ~span f Explicitness.Explicit rhs
              in
              (expr, rest)
          | None -> (var ~span:term.span name, rest))
      | Token { kind = Operator name; _ } -> (
          match
            Operator_env.find_prefix ~syntax_class:env.syntax_class
              env.operators name
          with
          | Some { Operator_env.expansion = Operator_env.Template template; _ }
            ->
              expand_syntax_template env term.span template (term :: rest)
          | Some op ->
              let rhs, rest = parse_expr_prec env op.precedence rest in
              let span = span_between term.span rhs.span in
              let f = var ~span:term.span name in
              let expr =
                match op.expansion with
                | Operator_env.Macro ->
                    stx ~span
                      (Syntax.MacroCall
                         ( f,
                           [ syntax_operator_arg ~span ~use_span:term.span op
                              [ rhs ] ] ))
                | Operator_env.Template _ ->
                    error
                      "internal error: template syntax should expand before \
                       operand parsing"
                | _ -> ap ~span f Explicitness.Explicit rhs
              in
              (expr, rest)
          | None -> unsupported ("unsupported prefix operator: " ^ name))
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
                parse_all (fun ts -> parse_expr_prec env 0 ts) cod_terms
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

and parse_expr_prec env min_prec terms =
  let lhs, rest = parse_primary env terms in
  parse_postfix_infix env min_prec lhs rest

and parse_postfix_infix env min_prec lhs terms =
  match terms with
  | term :: _ when is_separator term -> (lhs, terms)
  | term :: rest when token_kind ThinArrow term && min_prec <= 1 ->
      let rhs, rest = parse_expr_prec env 1 rest in
      let span = span_between lhs.span rhs.span in
      let lhs =
        match lhs.kind with
        | Syntax.Annotated { inner = { kind = Syntax.Var name; _ }; typ } ->
            stx ~span
              (Syntax.Arrow (Explicitness.Explicit, Some name, typ, None, rhs))
        | _ ->
            stx ~span
              (Syntax.Arrow (Explicitness.Explicit, None, lhs, None, rhs))
      in
      parse_postfix_infix env min_prec lhs rest
  | term :: rest
    when token_kind KwCan term
         && (min_prec <= 0
            || match lhs.kind with Syntax.Arrow _ -> true | _ -> false) ->
      let eff, rest = parse_can_effect_row env rest in
      parse_postfix_infix env min_prec (attach_effects lhs eff) rest
  | term :: rest when token_kind Colon term ->
      let typ, rest = parse_type_entry env rest in
      let lhs =
        stx
          ~span:(span_between lhs.span typ.span)
          (Syntax.Annotated { inner = lhs; typ })
      in
      parse_postfix_infix env min_prec lhs rest
  | at :: { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest
    when token_kind At at ->
      let args = match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args env items
      in
      let lhs =
        stx ~span:(span_between lhs.span span) (Syntax.MacroCall (lhs, args))
      in
      parse_postfix_infix env min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Paren, items, span); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "function call";
      let args =
        match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args env items
      in
      let call_span = span_between lhs.span term.span in
      let lhs =
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
          let arg = parse_all (fun ts -> parse_expr_prec env 0 ts) items in
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
          match
            Operator_env.find_infix ~syntax_class:env.syntax_class env.operators
              symbol
          with
          | Some op when op.precedence >= min_prec ->
              let next_min =
                match op.associativity with
                | Left -> op.precedence + 1
                | Right -> op.precedence
              in
              let rhs, rest = parse_expr_prec env next_min rest in
              let span = span_between lhs.span rhs.span in
              let lhs =
                match op.expansion with
                | Operator_env.BuiltinRefSet ->
                    stx ~span (Syntax.RefSet (lhs, rhs))
                | Template template ->
                    let branch = List.hd template.Syntax_template.branches in
                    let holes = Enforest_template.collect_pattern_holes branch.pattern in
                    let captures =
                      List.map2 (fun name operand ->
                        (name, { Syntax_template.syntax = operand; kind = Syntax_template.Expr; decl_terms = None }))
                        holes [ rhs; lhs ]
                    in
                    let parsed, _ = parse_expr_prec env 0 branch.replacement in
                    Enforest_template.substitute_template_captures captures parsed
                | Macro ->
                    let arg =
                      syntax_operator_arg ~span ~use_span:term.span op
                        [ lhs; rhs ]
                    in
                    arg
                | BuiltinApply ->
                    ap ~span
                      (ap ~span
                         (var ~span:term.span op.symbol)
                         Explicitness.Explicit lhs)
                      Explicitness.Explicit rhs
              in
              parse_postfix_infix env min_prec lhs rest
          | _ -> (lhs, term :: rest))
      | None -> (lhs, term :: rest))
  | [] -> (lhs, [])

and collect_do_body start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated do block"
    | term :: rest when token_kind KwDo term || token_kind KwSig term ->
        go (depth + 1) (term :: acc) rest
    | term :: rest
      when (token_kind KwStruct term || token_kind KwModule term)
           && not (starts_named_do_block rest) ->
        go (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, span_between start_span term.span)
        else go (depth - 1) (term :: acc) rest
    | term :: rest -> go depth (term :: acc) rest
  in
  go 0 [] terms

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
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: rest ->
      let _, value, rest = parse_fn env name_span rest in
      ensure_no_rest "function declaration" rest;
      Some
        {
          decl_name = id ~span:name_span name;
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
                   | Unsupported msg -> unsupported ("binding " ^ name_id.name ^ " type: " ^ msg)
                   | Error msg -> error ("binding " ^ name_id.name ^ " type: " ^ msg))
            | _ ->
                error
                  "binding parameters are not supported; use fn name(params) \
                   syntax"
          in
          let decl_value =
            try parse_all (fun ts -> parse_expr_prec env 0 ts) value_terms with
            | Unsupported msg -> unsupported ("binding " ^ name_id.name ^ ": " ^ msg)
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

and parse_type_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwType; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: rest -> (
      match split_at_token Equals rest with
      | Some
          ( param_terms,
            _,
            { datum = Group (Raw_syntax.Brace, field_terms, _); _ } :: [] ) ->
          let params =
            drop_separators param_terms
            |> List.concat_map (function
              | { datum = Token { kind = Ident p; _ }; span } -> [ id ~span p ]
              | { datum = Group (Paren, items, _); _ } ->
                  split_commas (drop_separators items)
                  |> List.map (fun ts ->
                      match drop_separators ts with
                      | [ { datum = Token { kind = Ident p; _ }; span } ] ->
                          id ~span p
                      | _ -> error "expected type parameter in parens")
              | _ -> error "expected type parameter")
          in
          let fields =
            split_statements field_terms
            |> List.map (fun field ->
                match drop_separators field with
                | { datum = Token { kind = Ident fname; _ }; _ }
                  :: colon :: typ_terms
                  when token_kind Colon colon ->
                    (fname, parse_type_terms env typ_terms)
                | _ -> error "expected record type field")
          in
          Some
            (Syntax.RecordTypeBinding
               { name = id ~span:name_span name; params; fields; public })
      | Some (param_terms, _, ctor_terms) ->
          let params =
            drop_separators param_terms
            |> List.concat_map (function
              | { datum = Token { kind = Ident p; _ }; span } -> [ id ~span p ]
              | { datum = Group (Paren, items, _); _ } ->
                  split_commas (drop_separators items)
                  |> List.map (fun ts ->
                      match drop_separators ts with
                      | [ { datum = Token { kind = Ident p; _ }; span } ] ->
                          id ~span p
                      | _ -> error "expected type parameter in parens")
              | _ -> error "expected type parameter")
          in
          let ctors =
            split_by_top_level_bar ctor_terms
            |> List.map (fun part ->
                match drop_separators part with
                | { datum = Token { kind = Ident cname; _ }; span }
                  :: payload_terms -> (
                    match drop_separators payload_terms with
                    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest
                      when List.exists (token_kind Comma) items ->
                        let rest = drop_separators rest in
                        if rest <> [] then
                          error "unexpected terms after constructor payload";
                        let types =
                          List.map
                            (fun ts -> parse_all (parse_type_entry env) ts)
                            (split_commas (drop_separators items))
                        in
                        (id ~span cname, types)
                    | _ ->
                        let payload =
                          match drop_separators payload_terms with
                          | [] -> []
                          | terms -> [ parse_type_terms env terms ]
                        in
                        (id ~span cname, payload))
                | _ -> error "expected constructor declaration")
          in
          Some
            (Syntax.TypeBinding
               { name = id ~span:name_span name; params; ctors; public })
      | None -> error "type binding requires =")
  | _ -> None

and parse_effect_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwEffect; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, op_terms) ->
          let params = parse_decl_type_params "effect" name_span param_terms in
          Some
            (Syntax.EffectBinding
               {
                 name = id ~span:name_span name;
                 params;
                 ops = parse_effect_ops env op_terms;
                 public;
               })
      | None -> error "effect binding requires =")
  | _ -> None

and parse_trait_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwTrait; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
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
               { name = id ~span:name_span name; params; fields; public })
      | None -> error "trait binding requires =")
  | _ -> None

and parse_impl_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwImpl; _ }; _ } :: rest -> (
      match split_at_token Equals rest with
      | Some (trait_terms, _, module_kw :: module_rest)
        when token_kind KwModule module_kw ->
          let (trait_path, trait_name), arg_terms =
            dotted_id_from_terms trait_terms
          in
          let args =
            match drop_separators arg_terms with
            | [ { datum = Group (Raw_syntax.Paren, items, _); _ } ] -> (
                let items = drop_separators items in
                if items = [] then error "impl argument list cannot be empty";
                match List.map (parse_type_terms env) (split_commas items) with
                | [ arg ] -> [ arg ]
                | _ ->
                    error "impl declaration accepts exactly one trait argument")
            | [] ->
                error "impl declaration requires a parenthesized trait argument"
            | _ -> error "impl trait argument must be written as (Type)"
          in
          let field_terms, after_struct, _ =
            collect_until_end module_kw.span module_rest
          in
          ensure_no_rest "impl binding" after_struct;
          let fields =
            split_statements field_terms
            |> List.map (fun field ->
                match parse_value_decl_statement env field with
                | Some { decl_name; decl_value; _ } ->
                    (decl_name.name, decl_value)
                | None -> error "expected impl let field")
          in
          Some
            (Syntax.ImplBinding { trait_path; trait_name; args; fields; public })
      | Some _ -> error "impl binding requires = module ... end"
      | None -> error "impl binding requires = module ... end")
  | _ -> None

and parse_open_statement stmt =
  match drop_separators stmt with
  | [
   { datum = Token { kind = KwOpen; _ }; _ };
   { datum = Token { kind = Ident name; _ }; span };
  ] ->
      Some (id ~span name)
  | _ -> None

and parse_import_statement env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwImport; _ }; _ } :: _ ->
      Some (parse_all (fun ts -> parse_expr_prec env 0 ts) stmt)
  | _ -> None

and parse_operator_value env start_span terms =
  let terms = drop_separators terms in
  let explicit_params, rest =
    match terms with
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        (parse_param_group env Explicitness.Explicit items, rest)
    | _ -> ([], terms)
  in
  let params = explicit_params in
  let body, rest, span =
    match drop_separators rest with
    | arrow :: body_terms when token_kind ThinArrow arrow ->
        let body = parse_all (fun ts -> parse_expr_prec env 0 ts) body_terms in
        (body, [], span_between start_span body.span)
    | do_kw :: body_rest when token_kind KwDo do_kw ->
        let body_terms, rest, span = collect_until_end do_kw.span body_rest in
        let body = parse_do_body_terms env span body_terms in
        (body, rest, span_between start_span span)
    | _ -> error "expected -> or do after operator parameters"
  in
  ( List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body,
    rest )

and parse_operator_template_decl env sym sym_span prec assoc value_terms =
  let terms = drop_separators value_terms in
  let hole_names, rest =
    match terms with
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        let holes =
          split_commas (drop_separators items)
          |> List.concat_map (fun ts ->
              match drop_separators ts with
              | [{ datum = Token { kind = Operator "$"; _ }; _ };
                 { datum = Token { kind = Ident name; _ }; span }] ->
                  [ (name, span) ]
              | _ -> error "operator template params must be $hole names")
        in
        (holes, rest)
    | _ -> error "operator template requires parameter list"
  in
  let holes = List.map fst hole_names in
  let body, rest, span =
    match drop_separators rest with
    | arrow :: body_terms when token_kind ThinArrow arrow ->
        let body = Enforest_template.rewrite_template_holes body_terms in
        (body, [], span_between sym_span (syntax_span body))
    | do_kw :: body_rest when token_kind KwDo do_kw ->
        let body_terms, rest, span = collect_until_end do_kw.span body_rest in
        let body = Enforest_template.rewrite_nested_syntax_body [] body_terms in
        (body, rest, span_between sym_span span)
    | _ -> error "expected -> or do after operator template parameters"
  in
  ensure_no_rest "infix template declaration" rest;
  let pattern =
    let hole name =
      Syntax_template.Hole { name; kind = Syntax_template.Expr; span = sym_span }
    in
    let op_literal = { datum = Token { kind = Operator sym; span = sym_span }; span = sym_span } in
    (match holes with
     | [ lhs; rhs ] -> [ hole lhs; Syntax_template.Literal op_literal; hole rhs ]
     | [ single ] -> [ Syntax_template.Literal op_literal; hole single ]
     | _ -> error "operator template must have 1 or 2 holes")
  in
  let template =
    { Syntax_template.head = sym;
      branches = [ { pattern; replacement = body; span } ];
      declaration_span = sym_span;
      inherited_captures = [] }
  in
  env.operators <-
    Operator_env.add_template_infix ~declaration_span:sym_span env.operators sym template prec assoc;
  Some (TemplateSyntaxDecl
          { syntax_name = id ~span:sym_span sym;
            syntax_export = Operator_env.template_infix ~declaration_span:sym_span sym template prec assoc })

and parse_operator_assoc assoc_str =
  match assoc_str with
  | "Left" -> Operator_env.Left
  | "Right" -> Operator_env.Right
  | _ -> error "operator infix associativity must be Left or Right"

and parse_syntax_template_decl env head_term head do_span body_rest =
  let body_terms, rest, _body_span = collect_until_end do_span body_rest in
  ensure_no_rest "syntax declaration" rest;
  load_imports_in_terms env body_terms;
  let inherited_captures = env.template_captures in
  let available = List.map fst inherited_captures in
  let branches = Enforest_template.parse_branches ~available head body_terms in
  let template =
    { Syntax_template.head; branches; declaration_span = head_term.span; inherited_captures }
  in
  env.operators <-
    Operator_env.add_template_prefix ~declaration_span:head_term.span
      env.operators head template 50;
  TemplateSyntaxDecl
    {
      syntax_name = id ~span:head_term.span head;
      syntax_export =
        Operator_env.template_prefix ~declaration_span:head_term.span head
          template 50;
    }

and template_callbacks env parse_decl =
    {
      Enforest_template.parse_expr =
        (fun terms -> parse_all (fun ts -> parse_expr_prec env 0 ts) terms);
      parse_expr_with_captures =
        (fun captures terms ->
          let previous = env.template_captures in
          env.template_captures <- captures @ previous;
          Fun.protect
            ~finally:(fun () -> env.template_captures <- previous)
            (fun () -> parse_all (fun ts -> parse_expr_prec env 0 ts) terms));
      parse_decl_with_captures =
        (fun captures terms ->
          let previous = env.template_captures in
          env.template_captures <- captures @ previous;
          Fun.protect
            ~finally:(fun () -> env.template_captures <- previous)
            (fun () -> parse_decl terms));
    }

and expand_syntax_template env use_span (template : Syntax_template.t) terms =
  Enforest_template.expand
    (template_callbacks env (fun _ ->
         error "declaration templates are not available in expression context"))
    use_span template terms

and expand_decl_syntax_template env parse_decl use_span (template : Syntax_template.t) terms =
  Enforest_template.expand_decl (template_callbacks env parse_decl) use_span template terms

and parse_decl_template_use env parse_decl stmt =
  match drop_separators stmt with
  | ({ datum = Token { kind = Ident head; _ }; span; _ } as _head_term) :: _ -> (
      match Operator_env.find_prefix ~syntax_class:env.syntax_class env.operators head with
      | Some { Operator_env.expansion = Operator_env.Template template; _ } ->
          let bindings, rest = expand_decl_syntax_template env parse_decl span template stmt in
          ensure_no_rest "declaration syntax template use" rest;
          Some bindings
      | _ -> None)
  | _ -> None

and parse_operator_shape stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident ifx; _ }; _ }
    :: { datum = Group (Raw_syntax.Paren, sym_items, sym_span); _ }
    :: { datum = Token { kind = Int p; _ }; _ }
    :: { datum = Token { kind = Ident assoc_str; _ }; span = assoc_span }
    :: value_terms
    when String.equal ifx "infix" ->
      let sym = match drop_separators sym_items with
        | [ term ] when Option.is_some (token_text term) ->
            Option.get (token_text term)
        | _ -> error "infix requires a symbol in parens"
      in
      let prec = Int64.to_int p in
      let assoc = parse_operator_assoc assoc_str in
      Some (`Infix (sym, sym_span, prec, assoc, assoc_span, value_terms))
  | _ -> None

and parse_operator_decl env stmt =
  match parse_operator_shape stmt with
  | Some (`Infix (name, name_span, prec, assoc, assoc_span, value_terms)) ->
      let is_template =
        match drop_separators value_terms with
        | { datum = Group (Raw_syntax.Paren, items, _); _ } :: _ ->
            let param_tokens = List.concat (split_commas (drop_separators items)) in
            let tokens = drop_separators param_tokens in
            let first_is_dollar = match tokens with
              | { datum = Token { kind = Operator "$"; _ }; _ } :: _ -> true
              | _ -> false
            in
            first_is_dollar
        | _ -> false
      in
      if is_template then
        parse_operator_template_decl env name name_span prec assoc value_terms
      else begin
        let value, rest = parse_operator_value env assoc_span value_terms in
        ensure_no_rest "infix declaration" rest;
        env.operators <-
          Operator_env.add_infix ~declaration_span:name_span env.operators name prec assoc;
        Some (MacroSyntaxDecl
                { syntax_name = id ~span:name_span name;
                  syntax_value = value;
                  syntax_export = Operator_env.macro_infix ~declaration_span:name_span name prec assoc })
      end
  | None -> (
      match drop_separators stmt with
      | { datum = Token { kind = Ident s; _ }; _ }
        :: head_term :: do_kw :: body_rest
        when String.equal s "syntax" && token_kind KwDo do_kw ->
          let head =
            match head_term.datum with
            | Token { kind = Ident name; _ } -> name
            | _ -> error "syntax declaration head must be an identifier"
          in
          Some
            (parse_syntax_template_decl env head_term head do_kw.span body_rest)
      | { datum = Token { kind = Ident s; _ }; _ } :: _
        when String.equal s "syntax" ->
          unsupported "unsupported syntax declaration shape"
      | _ -> None)

and parse_operator_decl_in_do env stmt =
  match parse_operator_shape stmt with
  | Some _ -> parse_operator_decl env stmt
  | None -> (
      match drop_separators stmt with
      | { datum = Token { kind = Ident s; _ }; _ } :: _
        when String.equal s "syntax" ->
          parse_operator_decl env stmt
      | _ -> None)

and scoped_binding_to_expr env span stmt body =
  let public, stmt = parse_public_prefix stmt in
  if public then error "pub is not supported inside do blocks";
  match parse_type_binding env false stmt with
  | Some (Syntax.TypeBinding { name; params; ctors; _ }) ->
      stx ~span (Syntax.TypeDef { name; params; ctors; body })
  | Some (Syntax.RecordTypeBinding { name; params; fields; _ }) ->
      stx ~span (Syntax.RecordTypeDef { name; params; fields; body })
  | Some _ -> error "unexpected non-type binding"
  | None -> (
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
                     { trait_path; trait_name; args; fields; _ }) ->
                  stx ~span
                    (Syntax.ImplDef
                       { trait_path; trait_name; args; fields; body })
              | Some _ -> error "unexpected non-impl binding"
              | None -> error "not a scoped binding")))

and parse_do_body_terms env span body_terms =
  with_operator_scope env (fun env ->
      let statements = split_statements body_terms in
      match List.rev statements with
      | [] -> error "empty do block"
      | final_stmt :: rev_statements ->
          let statements = List.rev rev_statements in
          let wrappers =
            List.map
              (fun stmt ->
                match parse_operator_decl_in_do env stmt with
                | Some (MacroSyntaxDecl { syntax_name = name; syntax_value = value; _ }) ->
                    fun acc -> stx ~span (Syntax.MacroDef { name; value; body = acc; kind = None })
                | Some (TemplateSyntaxDecl _) -> fun acc -> acc
                | None -> (
                    match parse_macro_binding env false stmt with
                    | Some (Syntax.MacroBinding { name; value; public = false; kind; _ }) ->
                        fun acc -> stx ~span (Syntax.MacroDef { name; value; body = acc; kind })
                    | Some (Syntax.MacroBinding { public = true; _ }) ->
                        error "pub macro is not supported inside do blocks"
                    | Some _ -> error "unexpected non-macro binding"
                    | None -> (
                        match parse_binding_statement env stmt with
                        | Some { decl_name = name; decl_type = type_; decl_value = value; decl_recursive = recursive } ->
                            fun acc -> stx ~span (Syntax.Let { name; type_; value; body = acc; recursive })
                        | None -> (
                            match parse_open_statement stmt with
                            | Some name -> fun acc -> stx ~span (Syntax.Open (name, acc))
                            | None -> (
                                match parse_import_statement env stmt with
                                | Some value ->
                                    fun acc -> stx ~span (Syntax.Let { name = id ~span:value.span "_"; type_ = None; value; body = acc; recursive = false })
                                | None ->
                                    fun acc -> scoped_binding_to_expr env span stmt acc))
                        )))
              statements
          in
          let body =
            parse_all (fun ts -> parse_expr_prec env 0 ts) final_stmt
          in
          List.fold_right (fun wrap acc -> wrap acc) wrappers body)

and parse_do env start_span terms =
  let body_terms, rest, span = collect_do_body start_span terms in
  (parse_do_body_terms env span body_terms, rest)

and parse_public_prefix stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwPub; _ }; _ } :: rest -> (true, rest)
  | rest -> (false, rest)

and parse_syntax_binding env public stmt =
  match parse_operator_decl env stmt with
  | Some (MacroSyntaxDecl { syntax_name = name; syntax_value = value; _ }) ->
      Some (Syntax.MacroBinding { name; value; public ; kind = None })
  | Some (TemplateSyntaxDecl _) -> None
  | None -> None

and parse_macro_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwMacro; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: rest ->
      let kind, value, rest = parse_fn ~kind_annotation:true env name_span rest in
      ensure_no_rest "macro binding" rest;
      Some
        (Syntax.MacroBinding { name = id ~span:name_span name; value; public; kind })
   | _ -> None

and parse_pattern_syn_binding _env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwPattern; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: { datum = Group (Paren, param_terms, _); _ } :: equals :: rhs_terms
    when token_kind Equals equals ->
      let params =
        split_commas (drop_separators param_terms)
        |> List.map (fun ts ->
            match drop_separators ts with
            | [ { datum = Token { kind = Ident p; _ }; span } ] -> id ~span p
            | _ -> error "expected pattern synonym parameter name")
      in
      let rhs = Enforest_pat.parse_pat_terms rhs_terms in
      Some (Syntax.PatternSynBinding { name = id ~span:name_span name; params; rhs; public })
  | _ -> None

and parse_macro_call_binding env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident name; _ }; span = name_span }
    :: { datum = Token { kind = At; _ }; _ }
    :: { datum = Group (Raw_syntax.Paren, items, span); _ }
    :: rest ->
      let args = match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args env items in
      ensure_no_rest "macro call binding" rest;
      let f = var ~span:name_span name in
      Some (Syntax.MacroCallBinding { f; args })
  | _ -> None

and parse_named_module_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwModule; _ }; span = module_span }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: do_kw :: body_rest
    when token_kind KwDo do_kw ->
      let body_terms, rest, span = collect_until_end do_kw.span body_rest in
      ensure_no_rest "module declaration" rest;
      let bindings = parse_module_bindings env body_terms in
      let value =
        stx ~span:(span_between module_span span) (Syntax.Module { bindings })
      in
      Some (Syntax.LetBinding { name = id ~span:name_span name; value; public; recursive = false })
  | _ -> None

and parse_value_binding env public stmt =
  match parse_value_decl_statement env stmt with
  | Some { decl_name = name; decl_type; decl_value; decl_recursive } ->
      let value =
        match decl_type with
        | Some typ ->
            stx ~span:(syntax_span stmt)
              (Syntax.Annotated { inner = decl_value; typ })
        | None -> decl_value
      in
      Some (Syntax.LetBinding { name; value; public; recursive = decl_recursive })
  | None -> None

and parse_module_binding env stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_operator_decl env stmt with
  | Some (TemplateSyntaxDecl { syntax_export; _ }) ->
      if public then env.exports_collector := syntax_export :: !(env.exports_collector);
      None
  | Some (MacroSyntaxDecl { syntax_name = name; syntax_value = value; syntax_export; _ }) ->
      if public then env.exports_collector := syntax_export :: !(env.exports_collector);
      Some (Syntax.MacroBinding { name; value; public ; kind = None })
  | None -> (
      match
        first_some
          [
             parse_macro_binding env public;
             parse_macro_call_binding env;
             parse_pattern_syn_binding env public;
             parse_type_binding env public;
             parse_effect_binding env public;
             parse_trait_binding env public;
             parse_impl_binding env public;
             parse_named_module_binding env public;
             parse_value_binding env public;
          ]
          stmt
      with
      | Some binding -> Some binding
      | None -> unsupported "unsupported Phase 7C module item")

and parse_module_statement env stmt =
  let parse_decl terms = parse_module_statement env terms in
  match parse_decl_template_use env parse_decl stmt with
  | Some bindings -> bindings
  | None -> (match parse_module_binding env stmt with Some binding -> [ binding ] | None -> [])

and parse_module_bindings env body_terms =
  with_operator_scope env (fun env ->
      split_statements body_terms |> List.concat_map (parse_module_statement env))

and collect_public_syntax_statement env stmt =
  let parse_decl terms =
    collect_public_syntax_statement env terms;
    []
  in
  match parse_decl_template_use env parse_decl stmt with
  | Some _ -> ()
  | None -> (
      let public, stmt = parse_public_prefix stmt in
      match parse_operator_decl env stmt with
      | Some (TemplateSyntaxDecl { syntax_export; _ }) ->
          if public then env.exports_collector := syntax_export :: !(env.exports_collector)
      | Some (MacroSyntaxDecl { syntax_export; _ }) ->
          if public then env.exports_collector := syntax_export :: !(env.exports_collector)
      | None -> ())

and collect_public_syntax_exports env body_terms =
  with_operator_scope env (fun env ->
      split_statements body_terms |> List.iter (collect_public_syntax_statement env))

and parse_struct_field env stmt =
  match drop_separators stmt with
  | [ { datum = Token { kind = Ident name; _ }; _ }; colon ]
    when token_kind Colon colon ->
      error ("missing type for struct field: " ^ name)
  | { datum = Token { kind = Ident name; _ }; _ } :: colon :: typ_terms
    when token_kind Colon colon ->
      if List.exists (token_kind Equals) typ_terms then None
      else Some (name, parse_type_terms env typ_terms)
  | _ -> None

and parse_named_struct_binding env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwStruct; _ }; span = struct_span }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
    :: do_kw :: body_rest
    when token_kind KwDo do_kw ->
      let body_terms, rest, span = collect_until_end do_kw.span body_rest in
      ensure_no_rest "struct declaration" rest;
      let con_fields, bindings = parse_struct_items env body_terms in
      let value =
        stx
          ~span:(span_between struct_span span)
          (Syntax.Struct { con_fields; bindings })
      in
      Some (Syntax.LetBinding { name = id ~span:name_span name; value; public; recursive = false })
  | _ -> None

and parse_struct_binding env stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_operator_decl env stmt with
  | Some (TemplateSyntaxDecl _) ->
      if public then error "pub syntax is not supported inside structs" else None
  | Some (MacroSyntaxDecl { syntax_name = name; syntax_value = value; _ }) ->
      if public then error "pub operator is not supported inside structs"
      else Some (Syntax.MacroBinding { name; value; public ; kind = None })
  | None -> (
      (match parse_macro_binding env public stmt with
      | Some _ when public -> error "pub macro is not supported inside structs"
      | Some binding -> Some binding
      | None -> (
          match parse_macro_call_binding env stmt with
          | Some _ when public -> error "pub macro call is not supported inside structs"
          | Some binding -> Some binding
          | None ->
      match
        first_some
          [
            parse_method_binding env public;
            parse_type_binding env public;
            parse_effect_binding env public;
            parse_trait_binding env public;
            parse_impl_binding env public;
            parse_named_module_binding env public;
            parse_named_struct_binding env public;
            parse_value_binding env public;
          ]
          stmt
      with
      | Some binding -> Some binding
      | None -> unsupported "unsupported Phase 7C struct item")))

and parse_struct_statement env stmt =
  let parse_decl terms = parse_struct_statement env terms in
  match parse_struct_field env stmt with
  | Some _ -> error "struct field declarations are deferred in declaration syntax templates"
  | None -> (
      match parse_decl_template_use env parse_decl stmt with
      | Some bindings -> bindings
      | None -> (
          match parse_struct_binding env stmt with Some binding -> [ binding ] | None -> []))

and parse_struct_items env body_terms =
  split_statements body_terms
  |> List.fold_left
       (fun (fields, bindings) stmt ->
         match parse_struct_field env stmt with
         | Some field -> (fields @ [ field ], bindings)
         | None -> (
              match parse_struct_statement env stmt with
              | [] -> (fields, bindings)
              | new_bindings -> (fields, bindings @ new_bindings)))
       ([], [])

let parse_terms env terms = parse_all (fun ts -> parse_expr_prec env 0 ts) terms

let parse_expr ?file ?load_syntax source =
  let env = env ?load_syntax () in
  try Raw_syntax.read ?file source |> parse_terms env
  with Raw_syntax.Error msg -> error msg

let parse_type ?file source =
  let env = env ~syntax_class:Syntax_class.TypeExpr () in
  try Raw_syntax.read ?file source |> parse_all (parse_type_entry env)
  with Raw_syntax.Error msg -> error msg

let parse_pat ?file source =
  try Raw_syntax.read ?file source |> parse_pat_terms
  with Raw_syntax.Error msg -> error msg

let parse_public_syntax_exports ?file ?load_syntax source =
  let env = env ?load_syntax () in
  try
    env.exports_collector := [];
    Raw_syntax.read ?file source |> collect_public_syntax_exports env;
    let exports = List.rev !(env.exports_collector) in
    (match Operator_env.duplicate_exports_message exports with
    | Some msg -> error msg
    | None -> ());
    exports
  with Raw_syntax.Error msg -> error msg

let parse_module ?file ?load_syntax source =
  let env = env ?load_syntax () in
  try
    let bindings = Raw_syntax.read ?file source |> parse_module_bindings env in
    stx (Syntax.Module { bindings })
  with Raw_syntax.Error msg -> error msg
