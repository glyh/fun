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

(* A call's arguments read as the kinds of the macro its head names, when any
   is not an [Expr] (M9); [None] otherwise, and the call is an application. *)
and macro_call_args env (head : Syntax.t) items =
  match env.macro_params head with
  | Some kinds when List.exists (fun k -> k <> Syntax.HoleExpr) kinds ->
      let macro = match head.kind with Syntax.Var id -> id.name | FieldAccess (_, f) -> f | _ -> "_" in
      let parts = match drop_separators items with [] -> [] | items -> split_commas items in
      if List.length parts <> List.length kinds then
        Expand_error.raise_at (ArgumentCount { macro; expected = List.length kinds; got = List.length parts });
      Some
        (List.map2
           (fun kind part ->
             match (kind : Syntax.hole_kind), drop_separators part with
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
             | (HoleId | HoleBlock | HolePattern | HoleDecl), _ ->
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
  | ({ datum = Token { kind = Operator "+"; _ }; _ } as op_term) :: rest ->
      let rhs, rest = parse_type_product env rest in
      let span = span_between lhs.span rhs.span in
      let lhs =
        ap ~span
          (ap ~span (var_of op_term "+") Explicitness.Explicit lhs)
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
  | ({ datum = Token { kind = Ident name; _ }; _ } as term) :: rest ->
      (var_of term name, rest)
  | ({ datum = Token { kind = KwUnit; _ }; _ } as term) :: rest ->
      (var_of term "Unit", rest)
  | { datum = Token { kind = KwSelfType; _ }; span } :: rest ->
      (stx ~span Syntax.SelfType, rest)
  | { datum = Token { kind = KwFn; _ }; span } :: rest ->
      let _, expr, rest = parse_fn env span rest in
      (expr, rest)
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
        if List.mem name Compiler_names.Type_name.parser_type_keywords then
          match terms with
          | term :: rest -> (var_of term name, rest)
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
  | [ ({ datum = Token { kind = Ident name; _ }; _ } as term) ] ->
      param_id explicitness (id_of term name)
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
      | { datum = Token { kind = Colon; _ }; _ } :: { datum = Token { kind = Ident "Decl"; _ }; _ } :: rest ->
          (Some Syntax.MacroAnnotation.Decl, None, rest)
      | { datum = Token { kind = Colon; _ }; _ } :: _ ->
          error "a macro annotation is : Expr(T), : Expr(_) or : Decl"
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
  let body, rest, span = parse_body env "fn parameters" rest in
  let span = span_between start_span span in
  (params, kind, output, body, rest, span)

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
  let params, kind, output, body, rest, span =
    parse_fn_parts ~kind_annotation env start_span terms
  in
  ( (kind, output),
    List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body,
    rest )

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
          let body, rest, _ = parse_body env "method parameters" rest in
          ensure_no_rest "method declaration" rest;
          Some
            (Syntax.MethodBinding
               { name = id_of name_term name; params; body; public })
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
  (stx ~span:(span_between start_span span) (Syntax.Module { bindings }), rest)

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
          | Same -> o.group_assoc = Syntax.RightAssoc
          | Unrelated -> no_order outer)
      | Some _, None -> false
      | None, Some _ -> true
      | None, None -> no_order outer)

and parse_postfix_infix env min_prec lhs terms =
  match terms with
  | term :: _ when is_separator term -> (lhs, terms)
  | term :: rest when token_kind ThinArrow term && (match min_prec with Top | ArrowRhs -> true | _ -> false) ->
      let rhs, rest = parse_expr_prec env ArrowRhs rest in
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
         && (min_prec = Top
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
                | Syntax.OrderGroup -> error ("an order group is not an operator: " ^ symbol)
              in
              parse_postfix_infix env min_prec lhs rest
          | _ -> (lhs, term :: rest))
      | None -> (lhs, term :: rest))
  | [] -> (lhs, [])

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

and parse_type_binding env public stmt =
  match drop_separators stmt with
  | ({ datum = Token { kind = KwType; _ }; _ } as type_kw) :: rest -> (
      match split_type_chain rest with
      | [] | [ _ ] -> parse_type_decl env public stmt
      | segments ->
          let members =
            List.map
              (fun segment ->
                match parse_type_decl env public (type_kw :: segment) with
                | Some (Syntax.TypeBinding { members = [ member ]; _ }) -> member
                | Some (Syntax.RecordTypeBinding _) -> error "record types cannot be part of an and chain"
                | _ -> error "expected type declaration in and chain")
              segments
          in
          let names = List.map (fun (m : Syntax.type_decl) -> m.name.name) members in
          (match List.find_opt (fun n -> List.length (List.filter (String.equal n) names) > 1) names with
           | Some dup -> error ("duplicate type in and chain: " ^ dup)
           | None -> ());
          Some (Syntax.TypeBinding { members; public }))
  | _ -> None

and parse_type_decl env public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwType; _ }; _ }
    :: ({ datum = Token { kind = Ident name; _ }; _ } as name_term)
    :: rest -> (
      match split_at_token Equals rest with
      | Some
          ( param_terms,
            _,
            [ { datum = Token { kind = KwStruct; _ }; _ }; { datum = Group (Raw_syntax.Brace, field_terms, _); _ } ] ) ->
          let params =
            drop_separators param_terms
            |> List.concat_map (function
              | ({ datum = Token { kind = Ident p; _ }; _ } as term) -> [ id_of term p ]
              | { datum = Group (Paren, items, _); _ } ->
                  split_commas (drop_separators items)
                  |> List.map (fun ts ->
                      match drop_separators ts with
                      | [ ({ datum = Token { kind = Ident p; _ }; _ } as term) ] ->
                          id_of term p
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
               { name = id_of name_term name; params; fields; public })
      | Some (_, _, [ { datum = Group (Raw_syntax.Brace, _, _); _ } ]) ->
          error "record types are written struct { field: Type }"
      | Some (param_terms, _, ctor_terms) ->
          let params =
            drop_separators param_terms
            |> List.concat_map (function
              | ({ datum = Token { kind = Ident p; _ }; _ } as term) -> [ id_of term p ]
              | { datum = Group (Paren, items, _); _ } ->
                  split_commas (drop_separators items)
                  |> List.map (fun ts ->
                      match drop_separators ts with
                      | [ ({ datum = Token { kind = Ident p; _ }; _ } as term) ] ->
                          id_of term p
                      | _ -> error "expected type parameter in parens")
              | _ -> error "expected type parameter")
          in
          let ctors =
            split_by_top_level_bar ctor_terms
            |> List.map (fun part ->
                match drop_separators part with
                | ({ datum = Token { kind = Ident cname; _ }; _ } as cname_term)
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
                        (id_of cname_term cname, types)
                    | _ ->
                        let payload =
                          match drop_separators payload_terms with
                          | [] -> []
                          | terms -> [ parse_type_terms env terms ]
                        in
                        (id_of cname_term cname, payload))
                | _ -> error "expected constructor declaration")
          in
          Some
            (Syntax.TypeBinding
               { members = [ { name = id_of name_term name; params; ctors } ]; public })
      | None -> error "type binding requires =")
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
          let trait, arg_terms = path_from_terms trait_terms in
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
   resolved by scope set, like any binder. *)
and resolve_order env term =
  match term.datum with
  | Token { kind = Ident name; _ } -> (
      match Binding.find_order env.operators ~scope:(token_scope term) name with
      | Some order -> order
      | None -> error ("unknown order group: " ^ name))
  | _ -> error "an order group is named by an identifier"

(* [order name : stronger_than(g, …), weaker_than(g, …), assoc(left|right)]:
   precedence is relative, and a group is related only by declarations. The
   order is transitive; a declaration that would make it cyclic is an error. *)
and parse_order_decl env name_term name clauses =
  let groups items =
    List.map
      (fun ts -> match drop_separators ts with [ t ] -> resolve_order env t | _ -> error "expected an order group")
      (split_commas items)
  in
  (* Clauses follow each other: a [,] would end the declaration's statement. *)
  let rec read_clauses (s, w, a) = function
    | [] -> (s, w, a)
    | { datum = Token { kind = Ident "stronger_than"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        read_clauses (s @ groups items, w, a) rest
    | { datum = Token { kind = Ident "weaker_than"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        read_clauses (s, w @ groups items, a) rest
    | { datum = Token { kind = Ident "assoc"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest -> (
        match drop_separators items with
        | [ { datum = Token { kind = Ident "left"; _ }; _ } ] -> read_clauses (s, w, Syntax.LeftAssoc) rest
        | [ { datum = Token { kind = Ident "right"; _ }; _ } ] -> read_clauses (s, w, Syntax.RightAssoc) rest
        | _ -> error "assoc is written assoc(left) or assoc(right)")
    | _ -> error "an order clause is stronger_than(…), weaker_than(…) or assoc(left|right)"
  in
  let stronger_than, weaker_than, group_assoc = read_clauses ([], [], Syntax.LeftAssoc) (drop_separators clauses) in
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
  let order = { Syntax.group = fresh_order_group name; group_name = name; group_assoc; stronger_than; weaker_than } in
  declare_role env (id_of name_term name)
    (Binding.role ~declared_at:name_term.span ~fixity:Syntax.PrefixOp ~order Syntax.OrderGroup)

and parse_syntax_template_decl env (head_id : Syntax.id) kind order body_terms rest =
  ensure_no_rest "syntax declaration" rest;
  let rules =
    Enforest_template.parse_rules ~available:env.holes ~head:head_id.name
      ~parse_replacement:(fun holes terms -> parse_replacement env kind holes terms)
      body_terms
  in
  declare_role env head_id
    (Binding.role ~declared_at:head_id.span ~fixity:Syntax.PrefixOp ?order
       (Syntax.Rules { rules_kind = kind; rules }))

and template_callbacks env trailing =
  { Enforest_template.parse_expr = (fun terms -> parse_all (fun ts -> parse_expr_prec env Top ts) terms);
    parse_expr_prefix = parse_expr_prec env;
    trailing;
    parse_pat_prefix = Enforest_pat.parse_pat_prefix;
    eager = env.eager }

(* A syntax form used where a declaration goes. *)
and parse_decl_template_use env stmt =
  match drop_separators stmt with
  | ({ datum = Token { kind = Ident head; _ }; _ } as head_term) :: _ -> (
      match Binding.find_role env.operators ~fixity:Syntax.PrefixOp ~scope:(token_scope head_term) head with
      | Some ({ meaning = Syntax.Rules { rules_kind; rules }; from_unit; _ } as role) ->
          let inst, rest =
            Enforest_template.instantiate (template_callbacks env (Operand (head, role))) ~form:(id_of head_term head) ~kind:rules_kind
              ~position:Syntax.MacroAnnotation.Decl ~from_unit rules stmt
          in
          ensure_no_rest "declaration syntax template use" rest;
          Some (Syntax.InstantiateBinding inst)
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
  | ({ datum = Token { kind = Ident _; _ }; _ } as term) :: rest -> (Some (resolve_order env term), rest)
  | rest -> (None, rest)

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
  match parse_type_binding env false stmt with
  | Some (Syntax.TypeBinding { members = [ { name; params; ctors } ]; _ }) ->
      stx ~span (Syntax.TypeDef { name; params; ctors; body })
  | Some (Syntax.TypeBinding _) ->
      error "and chains are not supported in a scoped do head; declare the chain as a do-body statement"
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

and parse_macro_call_binding env stmt =
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
         Syntax.MacroCallBinding { f; args }))
    env stmt

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
            parse_macro_binding env public;
            parse_macro_call_binding env;
            parse_pattern_syn_binding env public;
            parse_type_binding env public;
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
      match parse_decl_template_use env stmt with
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
          match parse_macro_call_binding env stmt with
          | Some _ when public ->
              error "pub macro call is not supported inside structs"
          | Some binding -> Some [ binding ]
          | None -> (
              match
                first_some
                  [
                    parse_open_binding env public;
                    parse_method_binding env public;
                    parse_type_binding env public;
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
          | Some (Syntax.InstantiateBinding inst) -> Some (inst, rest)
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
