open Raw_syntax
open Enforest_util

type callbacks = {
  parse_expr : Raw_syntax.t list -> Syntax.t;
  parse_expr_with_captures : (string * Syntax_template.captured) list -> Raw_syntax.t list -> Syntax.t;
  parse_decl_with_captures :
    (string * Syntax_template.captured) list -> Raw_syntax.t list -> Syntax.struct_binding list;
}

let intro_scope_counter = ref (-1)

let parse_template_hole_kind = function
  | "expr" -> Syntax_template.Expr
  | "binder" -> Syntax_template.Binder
  | "ident" -> Syntax_template.Ident
  | "decl" -> Syntax_template.Decl
  | kind -> error ("unknown syntax template hole kind: " ^ kind)

let raw_token_spelling term =
  match term.datum with
  | Token { kind = Ident s | Operator s; _ } -> Some s
  | Token { kind; _ } -> (match keyword_name kind with Some name -> Some name | None -> punct_name kind)
  | Group _ -> None

let same_literal_token expected actual =
  match (expected.datum, actual.datum) with
  | Token { kind = Int a; _ }, Token { kind = Int b; _ } -> Int64.equal a b
  | Token { kind = Char a; _ }, Token { kind = Char b; _ } -> Char.equal a b
  | Token { kind = String a; _ }, Token { kind = String b; _ } -> String.equal a b
  | Token { kind = Unit; _ }, Token { kind = Unit; _ } -> true
  | Token _, Token _ -> raw_token_spelling expected = raw_token_spelling actual
  | _ -> false

let parse_template_hole = function
  | { datum = Token { kind = Operator "$"; _ }; _ } :: { datum = Token { kind = Ident name; _ }; span } :: rest ->
      Some (Syntax_template.Hole { name; kind = Syntax_template.Expr; span }, rest)
  | { datum = Token { kind = Operator "$"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); span } :: rest -> (
      match drop_separators items with
      | [ { datum = Token { kind = Ident name; _ }; _ }; colon; { datum = Token { kind = Ident kind; _ }; _ } ]
        when token_kind Colon colon ->
          Some (Syntax_template.Hole { name; kind = parse_template_hole_kind kind; span }, rest)
      | _ -> error "expected template hole annotation $(name: kind)")
  | _ -> None

let rec parse_template_pattern_parts terms =
  let rec go acc = function
    | [] -> List.rev acc
    | terms -> (
        match parse_template_hole terms with
        | Some (part, rest) -> go (part :: acc) rest
        | None -> (
            match terms with
            | { datum = Group (delimiter, items, span); _ } :: rest ->
                go (Syntax_template.Group (delimiter, parse_template_pattern_parts items, span) :: acc) rest
            | term :: rest -> go (Syntax_template.Literal term :: acc) rest
            | [] -> List.rev acc))
  in
  go [] (drop_separators terms)

let template_pattern_starts_with head = function
  | Syntax_template.Literal term :: _ -> raw_token_spelling term = Some head
  | _ -> false

let collect_pattern_holes parts =
  let rec go acc = function
    | [] -> acc
    | Syntax_template.Hole { name; _ } :: rest ->
        if List.mem name acc then error ("duplicate syntax pattern hole: " ^ name);
        go (name :: acc) rest
    | Syntax_template.Group (_, parts, _) :: rest -> go (go acc parts) rest
    | Syntax_template.Literal _ :: rest -> go acc rest
  in
  go [] parts

let is_syntax_keyword = function
  | { datum = Token { kind = Ident name; _ }; _ } -> String.equal name "syntax"
  | _ -> false

let rec replacement_holes ?(bound = []) terms =
  let rec go acc = function
    | [] -> acc
    | terms -> (
        match parse_template_hole terms with
        | Some (Syntax_template.Hole { name; _ }, rest) ->
            let acc = if List.mem name bound then acc else name :: acc in
            go acc rest
        | Some _ -> assert false
        | None -> (
            match terms with
            | syntax_kw :: _head :: do_kw :: body_rest
              when is_syntax_keyword syntax_kw && token_kind KwDo do_kw ->
                let body_terms, rest, _ = collect_until_end do_kw.span body_rest in
                go (nested_replacement_holes bound acc body_terms) rest
            | { datum = Group (_, items, _); _ } :: rest ->
                go (replacement_holes ~bound items @ acc) rest
            | _ :: rest -> go acc rest
            | [] -> acc))
  in
  go [] terms

and nested_replacement_holes bound acc body_terms =
  split_match_branches body_terms
  |> List.fold_left
       (fun acc branch_terms ->
         match split_at_arrow branch_terms with
         | Some (pattern_terms, _, replacement) ->
             let inner_bound = collect_pattern_holes (parse_template_pattern_parts pattern_terms) in
             replacement_holes ~bound:(inner_bound @ bound) replacement @ acc
         | None -> acc)
       acc

let validate_template_replacement available captured replacement =
  List.iter
    (fun name ->
      if not (List.mem name (captured @ available)) then error ("unbound syntax template hole in replacement: " ^ name))
    (replacement_holes replacement)

let parse_branches ?(available = []) head body_terms =
  split_match_branches body_terms
  |> List.map (fun branch_terms ->
         match split_at_arrow branch_terms with
         | Some (pattern_terms, _, replacement) ->
              let pattern = parse_template_pattern_parts pattern_terms in
              if not (template_pattern_starts_with head pattern) then
                 error ("syntax branch pattern must start with declared head: " ^ head);
              let captured = collect_pattern_holes pattern in
              validate_template_replacement available captured replacement;
              { Syntax_template.pattern; replacement; span = syntax_span branch_terms }
         | None -> error "syntax declaration branch requires ->")

let rec match_template_group callbacks captures pattern_items input_items =
  match match_template_parts callbacks captures pattern_items input_items with
  | Some (captures, rest) when drop_separators rest = [] -> Some captures
  | _ -> None

and match_template_parts callbacks captures pattern input =
  match pattern with
  | [] -> Some (captures, input)
  | Syntax_template.Literal expected :: rest -> (
      match drop_separators input with
      | actual :: input_rest when same_literal_token expected actual -> match_template_parts callbacks captures rest input_rest
      | _ -> None)
  | Syntax_template.Group (delimiter, pattern_items, _) :: rest -> (
      match drop_separators input with
      | { datum = Group (actual_delimiter, input_items, _); _ } :: input_rest when actual_delimiter = delimiter -> (
          match match_template_group callbacks captures pattern_items input_items with
          | Some captures -> match_template_parts callbacks captures rest input_rest
          | None -> None)
      | _ -> None)
  | Syntax_template.Hole { name; kind; _ } :: rest -> (
      match kind with
      | Syntax_template.Binder | Syntax_template.Ident -> (
          match drop_separators input with
          | ({ datum = Token { kind = Ident _; _ }; span; _ } as term) :: input_rest ->
              let captured =
                {
                  Syntax_template.syntax = var ~span (Option.get (raw_token_spelling term));
                  kind;
                  decl_terms = None;
                }
              in
              match_template_parts callbacks ((name, captured) :: captures) rest input_rest
          | _ -> None)
      | Syntax_template.Expr ->
          let rec try_prefix prefix = function
            | [] -> None
            | term :: input_rest ->
                let prefix = prefix @ [ term ] in
                let candidate =
                  try
                    let expr = callbacks.parse_expr prefix in
                    let captured =
                      { Syntax_template.syntax = expr; kind = Syntax_template.Expr; decl_terms = None }
                    in
                    match match_template_parts callbacks ((name, captured) :: captures) rest input_rest with
                    | Some result -> Some result
                    | None -> None
                  with Error _ | Unsupported _ -> None
                in
                (match candidate with Some _ -> candidate | None -> try_prefix prefix input_rest)
          in
          try_prefix [] (drop_separators input)
      | Syntax_template.Decl ->
          if rest = [] then
            let input = drop_separators input in
            if input = [] then None
            else
              let captured =
                {
                  Syntax_template.syntax = stx ~span:(syntax_span input) (Syntax.Module { bindings = [] });
                  kind = Syntax_template.Decl;
                  decl_terms = Some input;
                }
              in
              Some ((name, captured) :: captures, [])
          else
          let rec try_prefix prefix = function
            | [] ->
                let prefix = drop_separators prefix in
                if prefix = [] then None
                else
                  let captured =
                    {
                      Syntax_template.syntax = stx ~span:(syntax_span prefix) (Syntax.Module { bindings = [] });
                      kind = Syntax_template.Decl;
                      decl_terms = Some prefix;
                    }
                  in
                  match_template_parts callbacks ((name, captured) :: captures) rest []
            | term :: input_rest ->
                let prefix = prefix @ [ term ] in
                let prefix' = drop_separators prefix in
                let candidate =
                  if prefix' = [] then None
                  else
                    let captured =
                      {
                        Syntax_template.syntax = stx ~span:(syntax_span prefix') (Syntax.Module { bindings = [] });
                        kind = Syntax_template.Decl;
                        decl_terms = Some prefix';
                      }
                    in
                    match match_template_parts callbacks ((name, captured) :: captures) rest input_rest with
                    | Some result -> Some result
                    | None -> None
                in
                (match candidate with Some _ -> candidate | None -> try_prefix prefix input_rest)
          in
          try_prefix [] (drop_separators input))

let placeholder_name name = "__syntax_template_hole_" ^ name

let placeholder_prefix = placeholder_name ""

let placeholder_id_name name =
  if String.starts_with ~prefix:placeholder_prefix name then
    Some
      (String.sub name (String.length placeholder_prefix)
         (String.length name - String.length placeholder_prefix))
  else None

let lookup_capture captures name position =
  match List.assoc_opt name captures with
  | Some captured -> captured
  | None -> error ("unbound syntax template hole in " ^ position ^ ": " ^ name)

let captured_id captures name position =
  let captured = lookup_capture captures name position in
  match (position, captured.Syntax_template.kind, captured.syntax.kind) with
  | "binder", Syntax_template.Binder, Syntax.Var id -> id
  | "binder", Syntax_template.Expr, Syntax.Var _ ->
      error ("expression hole used in binder position: " ^ name)
  | "binder", Syntax_template.Ident, Syntax.Var _ ->
      error ("identifier hole used in binder position: " ^ name)
  | "reference", Syntax_template.Ident, Syntax.Var id -> id
  | "reference", Syntax_template.Binder, Syntax.Var _ ->
      error ("binder hole used in reference position: " ^ name)
  | "reference", Syntax_template.Expr, Syntax.Var _ ->
      error ("expression hole used in reference position: " ^ name)
  | _, _, _ -> error ("syntax template hole is not an identifier: " ^ name)

let map_binder_id captures (id : Syntax.id) =
  match placeholder_id_name id.Syntax.name with
  | Some name -> captured_id captures name "binder"
  | None -> id

let map_reference_id captures (id : Syntax.id) =
  match placeholder_id_name id.Syntax.name with
  | Some name -> captured_id captures name "reference"
  | None -> id

let map_param captures (p : Syntax.param) =
  { p with name = map_binder_id captures p.name }

let rec rewrite_template_holes ?(bound = []) terms =
  let rec go acc = function
    | [] -> List.rev acc
    | terms -> (
        match terms with
        | ({ datum = Token { kind = Operator "$"; _ }; _ } as dollar)
          :: ({ datum = Token { kind = Ident name; _ }; span } as ident) :: rest ->
            if List.mem name bound then go (ident :: dollar :: acc) rest
            else
              let term = { datum = Token { kind = Ident (placeholder_name name); span }; span } in
              go (term :: acc) rest
        | ({ datum = Token { kind = Operator "$"; _ }; _ } as dollar)
          :: ({ datum = Group (Raw_syntax.Paren, items, span); _ } as group) :: rest -> (
            match drop_separators items with
            | [ { datum = Token { kind = Ident name; _ }; _ }; colon; { datum = Token { kind = Ident _kind; _ }; _ } ]
              when token_kind Colon colon ->
                if List.mem name bound then go (group :: dollar :: acc) rest
                else
                  let term = { datum = Token { kind = Ident (placeholder_name name); span }; span } in
                  go (term :: acc) rest
            | _ -> error "expected template hole annotation $(name: kind)")
        | syntax_kw :: head :: do_kw :: body_rest
          when is_syntax_keyword syntax_kw && token_kind KwDo do_kw ->
            let body_terms, rest, body_span = collect_until_end do_kw.span body_rest in
            let rewritten_body = rewrite_nested_syntax_body bound body_terms in
            let end_term = { datum = Token { kind = KwEnd; span = body_span }; span = body_span } in
            go (List.rev_append (syntax_kw :: head :: do_kw :: rewritten_body @ [ end_term ]) acc) rest
        | { datum = Group (delimiter, items, span); _ } :: rest ->
            go ({ datum = Group (delimiter, rewrite_template_holes ~bound items, span); span } :: acc) rest
        | term :: rest -> go (term :: acc) rest
        | [] -> List.rev acc)
  in
  go [] terms

and rewrite_nested_syntax_body bound body_terms =
  let branch_separator () =
    let span = Source_span.synthetic in
    { datum = Token { kind = Bar; span }; span }
  in
  let rec join_branches = function
    | [] -> []
    | [ branch ] -> branch
    | branch :: rest -> branch @ (branch_separator () :: join_branches rest)
  in
  split_match_branches body_terms
  |> List.map (fun branch_terms ->
         match split_at_arrow branch_terms with
         | Some (pattern_terms, arrow, replacement) ->
             let inner_bound = collect_pattern_holes (parse_template_pattern_parts pattern_terms) in
             pattern_terms @ (arrow :: rewrite_template_holes ~bound:(inner_bound @ bound) replacement)
         | None -> branch_terms)
  |> join_branches

let fresh_intro_scope () =
  let scope = !intro_scope_counter in
  decr intro_scope_counter;
  Scope_set.singleton scope

let rec substitute_template_captures captures (stx : Syntax.t) =
  let go = substitute_template_captures captures in
  match stx.kind with
  | Syntax.Var id when Option.is_some (placeholder_id_name id.name) ->
      let name = Option.get (placeholder_id_name id.name) in
      let captured = lookup_capture captures name "replacement" in
      (match captured.Syntax_template.kind with
      | Syntax_template.Binder -> error ("binder hole used in expression position: " ^ name)
      | Syntax_template.Decl -> error ("declaration hole used in expression position: " ^ name)
      | Syntax_template.Expr | Syntax_template.Ident -> captured.Syntax_template.syntax)
  | Syntax.Atom _ | Syntax.Var _ | Syntax.Self | Syntax.SelfType | Syntax.Import _ -> stx
  | Syntax.Ap (f, e, a) -> { stx with kind = Syntax.Ap (go f, e, go a) }
  | Syntax.Lam (p, body) ->
      { stx with kind = Syntax.Lam ({ (map_param captures p) with type_ = Option.map go p.type_ }, go body) }
  | Syntax.Let { name; type_; value; body; recursive } ->
      { stx with kind = Syntax.Let { name = map_binder_id captures name; type_ = Option.map go type_; value = go value; body = go body; recursive } }
  | Syntax.If { cond; then_; else_ } -> { stx with kind = Syntax.If { cond = go cond; then_ = go then_; else_ = go else_ } }
  | Syntax.Annotated { inner; typ } -> { stx with kind = Syntax.Annotated { inner = go inner; typ = go typ } }
  | Syntax.Prod xs -> { stx with kind = Syntax.Prod (List.map go xs) }
  | Syntax.ProdTy xs -> { stx with kind = Syntax.ProdTy (List.map go xs) }
  | Syntax.Arrow (expl, name, dom, eff, cod) ->
      let eff = Option.map (fun (row : Syntax.effect_row) -> { Syntax.effects = List.map go row.effects; tail = Option.map go row.tail }) eff in
      { stx with kind = Syntax.Arrow (expl, Option.map (map_binder_id captures) name, go dom, eff, go cod) }
  | Syntax.FieldAccess (e, n) -> { stx with kind = Syntax.FieldAccess (go e, n) }
  | Syntax.Proj (e, n) -> { stx with kind = Syntax.Proj (go e, n) }
  | Syntax.RecordConstruct { typ; fields } ->
      { stx with kind = Syntax.RecordConstruct { typ = go typ; fields = List.map (fun (n, e) -> (n, go e)) fields } }
  | Syntax.Struct { con_fields; bindings } ->
      { stx with kind = Syntax.Struct { con_fields = List.map (fun (n, e) -> (n, go e)) con_fields; bindings = List.map (map_template_struct_binding captures go) bindings } }
  | Syntax.Module { bindings } -> { stx with kind = Syntax.Module { bindings = List.map (map_template_struct_binding captures go) bindings } }
  | Syntax.Open (m, body) -> { stx with kind = Syntax.Open (map_reference_id captures m, go body) }
  | Syntax.RecordTypeDef { name; params; fields; body } ->
      { stx with kind = Syntax.RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body } }
  | Syntax.TypeDef { name; params; ctors; body } ->
      { stx with kind = Syntax.TypeDef { name; params; ctors = List.map (fun (n, ps) -> (n, List.map go ps)) ctors; body = go body } }
  | Syntax.EffectDef { name; params; ops; body } ->
      { stx with kind = Syntax.EffectDef { name; params; ops = List.map (fun (op : Syntax.effect_op) -> { op with input = go op.input; output = go op.output }) ops; body = go body } }
  | Syntax.TraitDef { name; params; fields; body } ->
      { stx with kind = Syntax.TraitDef { name; params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body } }
  | Syntax.ImplDef { trait_path; trait_name; args; fields; body } ->
      { stx with kind = Syntax.ImplDef { trait_path; trait_name; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body } }
  | Syntax.Perform { effect_path; op; arg } -> { stx with kind = Syntax.Perform { effect_path; op; arg = go arg } }
  | Syntax.Resume e -> { stx with kind = Syntax.Resume (go e) }
  | Syntax.RefNew e -> { stx with kind = Syntax.RefNew (go e) }
  | Syntax.RefGet e -> { stx with kind = Syntax.RefGet (go e) }
  | Syntax.RefSet (l, r) -> { stx with kind = Syntax.RefSet (go l, go r) }
  | Syntax.Match (scrut, branches) -> { stx with kind = Syntax.Match (go scrut, List.map (map_template_match_branch captures go) branches) }
  | Syntax.MacroDef { name; value; body } -> { stx with kind = Syntax.MacroDef { name = map_binder_id captures name; value = go value; body = go body } }
  | Syntax.MacroCall (f, a) -> { stx with kind = Syntax.MacroCall (go f, go a) }
  | Syntax.SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span } ->
      { stx with kind = Syntax.SyntaxOperatorUse { operator; fixity; operands = List.map go operands; declaration_span; use_span } }

and map_template_struct_binding captures go = function
  | Syntax.LetBinding { name; value; public } -> Syntax.LetBinding { name = map_binder_id captures name; value = go value; public }
  | Syntax.MethodBinding { name; params; body; public } -> Syntax.MethodBinding { name = map_binder_id captures name; params = List.map (map_param captures) params; body = go body; public }
  | Syntax.TypeBinding { name; params; ctors; public } -> Syntax.TypeBinding { name = map_binder_id captures name; params = List.map (map_binder_id captures) params; ctors = List.map (fun (n, ps) -> (map_binder_id captures n, List.map go ps)) ctors; public }
  | Syntax.RecordTypeBinding { name; params; fields; public } -> Syntax.RecordTypeBinding { name = map_binder_id captures name; params = List.map (map_binder_id captures) params; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
  | Syntax.EffectBinding { name; params; ops; public } -> Syntax.EffectBinding { name = map_binder_id captures name; params = List.map (map_binder_id captures) params; ops = List.map (fun (op : Syntax.effect_op) -> { op with input = go op.input; output = go op.output }) ops; public }
  | Syntax.TraitBinding { name; params; fields; public } -> Syntax.TraitBinding { name = map_binder_id captures name; params = List.map (map_binder_id captures) params; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
  | Syntax.ImplBinding { trait_path; trait_name; args; fields; public } -> Syntax.ImplBinding { trait_path; trait_name; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
  | Syntax.MacroBinding { name; value; public } -> Syntax.MacroBinding { name = map_binder_id captures name; value = go value; public }

and map_template_match_branch captures go = function
  | Syntax.ValueBranch (pat, body) -> Syntax.ValueBranch (map_template_pat captures pat, go body)
  | Syntax.EffectBranch { effect_path; op; arg_pat; body } -> Syntax.EffectBranch { effect_path; op; arg_pat = map_template_pat captures arg_pat; body = go body }

and map_template_pat captures = function
  | Syntax.PatCon (path, name, pats) -> Syntax.PatCon (path, name, List.map (map_template_pat captures) pats)
  | Syntax.PatRecord { typ_path; typ; fields; partial } ->
      Syntax.PatRecord { typ_path; typ; fields = List.map (fun (field, pat) -> (field, Option.map (map_template_pat captures) pat)) fields; partial }
  | Syntax.PatStructType { fields; partial } ->
      Syntax.PatStructType { fields = List.map (fun (field, pat) -> (field, map_template_pat captures pat)) fields; partial }
  | Syntax.PatOr (lhs, rhs) -> Syntax.PatOr (map_template_pat captures lhs, map_template_pat captures rhs)
  | Syntax.PatProd pats -> Syntax.PatProd (List.map (map_template_pat captures) pats)
  | Syntax.PatBind id -> Syntax.PatBind (map_binder_id captures id)
  | (Syntax.PatAtom _ | Syntax.PatType _ | Syntax.PatWild) as pat -> pat

let is_multi_block replacement =
  match drop_separators replacement with
  | { datum = Token { kind = Ident "multi"; _ }; _ } :: _ -> true
  | _ -> false

let instantiate_template_replacement callbacks captures replacement =
  (match drop_separators replacement with
  | _ when is_multi_block replacement ->
      error "multi ... end is only valid in declaration syntax templates"
  | _ -> ());
  let rewritten = rewrite_template_holes replacement in
  let parsed = callbacks.parse_expr_with_captures captures rewritten in
  let introduced = Expand.add_scope (fresh_intro_scope ()) parsed in
  substitute_template_captures captures introduced

let captured_decl_terms captures name =
  let captured = lookup_capture captures name "declaration replacement" in
  match (captured.Syntax_template.kind, captured.decl_terms) with
  | Syntax_template.Decl, Some terms -> terms
  | Syntax_template.Decl, None -> error ("declaration hole has no captured declaration terms: " ^ name)
  | _ -> error ("non-declaration hole used as declaration: " ^ name)

let rec rewrite_decl_template_holes captures terms =
  let rec go acc = function
    | [] -> List.rev acc
    | { datum = Token { kind = Operator "$"; _ }; _ }
      :: { datum = Token { kind = Ident name; _ }; span } :: rest -> (
        match List.assoc_opt name captures with
        | Some { Syntax_template.kind = Syntax_template.Decl; _ } ->
            go (List.rev_append (captured_decl_terms captures name) acc) rest
        | _ ->
            let term = { datum = Token { kind = Ident (placeholder_name name); span }; span } in
            go (term :: acc) rest)
    | { datum = Token { kind = Operator "$"; _ }; _ }
      :: { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest -> (
        match drop_separators items with
        | [ { datum = Token { kind = Ident name; _ }; _ }; colon; { datum = Token { kind = Ident _kind; _ }; _ } ]
          when token_kind Colon colon -> (
            match List.assoc_opt name captures with
            | Some { Syntax_template.kind = Syntax_template.Decl; _ } ->
                go (List.rev_append (captured_decl_terms captures name) acc) rest
            | _ ->
                let term = { datum = Token { kind = Ident (placeholder_name name); span }; span } in
                go (term :: acc) rest)
        | _ -> error "expected template hole annotation $(name: kind)")
    | { datum = Group (delimiter, items, span); _ } :: rest ->
        go ({ datum = Group (delimiter, rewrite_decl_template_holes captures items, span); span } :: acc) rest
    | term :: rest -> go (term :: acc) rest
  in
  go [] terms

let declaration_replacement_statements replacement =
  match drop_separators replacement with
  | { datum = Token { kind = Ident "multi"; _ }; span = multi_span } :: body_terms ->
      let body, rest, _span = collect_until_end multi_span body_terms in
      ensure_no_rest "multi declaration template" rest;
      split_statements body
  | terms -> [ terms ]

let instantiate_decl_template_replacement callbacks captures replacement =
  let rewritten = rewrite_decl_template_holes captures replacement in
  let statements = declaration_replacement_statements rewritten in
  let intro_scope = fresh_intro_scope () in
  statements
  |> List.concat_map (fun stmt ->
         try callbacks.parse_decl_with_captures captures stmt with
         | Unsupported msg -> unsupported ("declaration template replacement: " ^ msg)
         | Error msg -> error ("declaration template replacement: " ^ msg))
  |> List.map (fun binding ->
         binding
         |> Expand.add_struct_binding_scopes [ intro_scope ]
         |> map_template_struct_binding captures (substitute_template_captures captures))

let expand callbacks _use_span (template : Syntax_template.t) terms =
  let rec try_branches = function
    | [] -> error ("no matching branch for syntax " ^ template.head)
    | branch :: rest -> (
        match match_template_parts callbacks [] branch.Syntax_template.pattern terms with
        | Some (captures, remaining) ->
            let captures = captures @ template.inherited_captures in
            let expanded = instantiate_template_replacement callbacks captures branch.replacement in
            (expanded, remaining)
        | None -> try_branches rest)
  in
  try_branches template.branches

let expand_decl callbacks _use_span (template : Syntax_template.t) terms =
  let rec try_branches = function
    | [] -> error ("no matching branch for syntax " ^ template.head)
    | branch :: rest -> (
        match match_template_parts callbacks [] branch.Syntax_template.pattern terms with
        | Some (captures, remaining) ->
            let captures = captures @ template.inherited_captures in
            let expanded = instantiate_decl_template_replacement callbacks captures branch.replacement in
            (expanded, remaining)
        | None -> try_branches rest)
  in
  try_branches template.branches
