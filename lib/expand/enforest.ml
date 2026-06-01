exception Unsupported of string
exception Error of string

let unsupported msg = raise (Unsupported msg)
let error msg = raise (Error msg)

open Raw_syntax

let id ?span name =
  match span with
  | Some span -> Syntax.fresh_id ~span name
  | None -> Syntax.fresh_id name

let stx ?(span = Source_span.synthetic) kind = { Syntax.kind; span }

let atom ?span atom = stx ?span (Syntax.Atom atom)
let var ?span name = stx ?span (Syntax.Var (id ?span name))

let span_between (a : Source_span.t) (b : Source_span.t) =
  if a.synthetic || b.synthetic then Source_span.synthetic
  else
    Source_span.make ?file:a.file ~start_byte:a.start_byte ~end_byte:b.end_byte
      ?start_line:a.start_line ?start_col:a.start_col ?end_line:b.end_line
      ?end_col:b.end_col ()

let syntax_span (terms : Raw_syntax.t list) =
  match terms with
  | [] -> Source_span.synthetic
  | first :: rest ->
      let last = List.fold_left (fun _ t -> t) first rest in
      span_between first.span last.span

let is_separator (term : Raw_syntax.t) =
  match term.datum with
  | Token { kind = Semi; _ } -> true
  | _ -> false

let rec drop_separators = function
  | term :: rest when is_separator term -> drop_separators rest
  | terms -> terms

let token_text (term : Raw_syntax.t) =
  match term.datum with
  | Token { kind = Ident s | Operator s; _ } -> Some s
  | _ -> None

let token_kind kind term =
  match term.datum with
  | Token { kind = k; _ } -> k = kind
  | _ -> false

let keyword_name = function
  | KwFn -> Some "fn"
  | KwEnd -> Some "end"
  | KwIf -> Some "if"
  | KwElse -> Some "else"
  | KwMatch -> Some "match"
  | KwWith -> Some "with"
  | KwEffect -> Some "effect"
  | KwType -> Some "type"
  | KwModule -> Some "module"
  | KwStruct -> Some "struct"
  | KwImpl -> Some "impl"
  | KwTrait -> Some "trait"
  | KwPub -> Some "pub"
  | KwImport -> Some "import"
  | KwOpen -> Some "open"
  | KwMacro -> Some "macro"
  | KwSelf -> Some "self"
  | KwSelfType -> Some "Self"
  | KwRef -> Some "ref"
  | KwDeref -> Some "deref"
  | KwRec -> Some "rec"
  | KwCan -> Some "can"
  | KwPerform -> Some "perform"
  | KwResume -> Some "resume"
  | KwMethod -> Some "method"
  | _ -> None

let punct_name = function
  | LParen -> Some "("
  | RParen -> Some ")"
  | LBracket -> Some "["
  | RBracket -> Some "]"
  | LBrace -> Some "{"
  | RBrace -> Some "}"
  | Comma -> Some ","
  | Dot -> Some "."
  | Colon -> Some ":"
  | Equals -> Some "="
  | Semi -> Some ";"
  | Bar -> Some "|"
  | ThinArrow -> Some "->"
  | At -> Some "@"
  | DatumComment -> Some "#_"
  | _ -> None

let ap ?span f explicitness arg = stx ?span (Syntax.Ap (f, explicitness, arg))

let unit ?span () = atom ?span Atom.Unit

let unit_type ?span () = var ?span "Unit"

let param ?span ?type_ explicitness name =
  { Syntax.name = id ?span name; type_; trait_bounds = []; explicitness }

let split_commas terms =
  let rec go current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when token_kind Comma term -> go [] (List.rev current :: acc) rest
    | term :: rest -> go (term :: current) acc rest
  in
  go [] [] terms

let parse_all parse terms =
  let terms = drop_separators terms in
  match terms with
  | [] -> error "expected expression"
  | _ ->
      let expr, rest = parse terms in
      let rest = drop_separators rest in
      if rest = [] then expr else unsupported "unconsumed terms after expression"

let rec split_at_pred pred acc = function
  | [] -> None
  | term :: rest when pred term -> Some (List.rev acc, term, rest)
  | term :: rest -> split_at_pred pred (term :: acc) rest

let split_at_token kind terms = split_at_pred (token_kind kind) [] terms

let split_at_arrow terms = split_at_token ThinArrow terms

let split_by_top_level_bar terms =
  let rec go current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when token_kind Bar term -> go [] (List.rev current :: acc) rest
    | term :: rest -> go (term :: current) acc rest
  in
  go [] [] terms |> List.filter (fun part -> part <> [])

let collect_until_end start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated block"
    | term :: rest when token_kind KwDo term -> go (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, span_between start_span term.span)
        else go (depth - 1) (term :: acc) rest
    | term :: rest -> go depth (term :: acc) rest
  in
  go 0 [] terms

let rec parse_args terms =
  match split_commas terms with
  | [ [] ] -> []
  | parts ->
      List.map
        (fun part ->
          if List.for_all is_separator part then error "empty argument";
          parse_all (fun ts -> parse_expr_prec 0 ts) part)
        parts

and parse_group_expr delimiter items span =
  match delimiter with
  | Raw_syntax.Paren -> (
      let items = drop_separators items in
      match items with
      | [] -> unit ~span ()
      | _ -> (
          match split_commas items with
          | [ only ] -> parse_all (fun ts -> parse_expr_prec 0 ts) only
          | parts ->
              let exprs = List.map (parse_all (fun ts -> parse_expr_prec 0 ts)) parts in
              stx ~span (Syntax.Prod exprs)))
  | Bracket -> unsupported "bare bracket expression is not in Phase 7A"
  | Brace -> unsupported "bare brace expression is not in Phase 7A"

and parse_type_terms terms = parse_all (fun ts -> parse_expr_prec 0 ts) terms

and parse_param_item explicitness terms =
  let terms = drop_separators terms in
  match terms with
  | [] when explicitness = Explicitness.Explicit ->
      param ~type_:(unit_type ()) Explicitness.Explicit "_"
  | [ { datum = Token { kind = Ident name; _ }; span } ] ->
      param ~span explicitness name
  | { datum = Token { kind = Ident name; _ }; span } :: colon :: typ_terms when token_kind Colon colon ->
      param ~span ~type_:(parse_type_terms typ_terms) explicitness name
  | _ -> error "expected parameter of the form name or name : Type"

and parse_param_group explicitness items =
  let items = drop_separators items in
  match items with
  | [] when explicitness = Explicitness.Explicit ->
      [ param ~type_:(unit_type ()) Explicitness.Explicit "_" ]
  | [] -> error "empty implicit parameter list"
  | _ -> List.map (parse_param_item explicitness) (split_commas items)

and parse_fn start_span terms =
  let rec collect_params saw_explicit acc = function
    | { datum = Group (Raw_syntax.Bracket, items, _); _ } :: rest ->
        if saw_explicit then error "implicit fn parameters must precede explicit parameters";
        collect_params saw_explicit (acc @ parse_param_group Explicitness.Implicit items) rest
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        collect_params true (acc @ parse_param_group Explicitness.Explicit items) rest
    | rest -> (acc, rest)
  in
  let params, rest = collect_params false [] (drop_separators terms) in
  let params = if params = [] then error "fn requires a parameter list" else params in
  let body, rest, span =
    match drop_separators rest with
    | arrow :: body_terms when token_kind ThinArrow arrow ->
        let body = parse_all (fun ts -> parse_expr_prec 0 ts) body_terms in
        (body, [], span_between start_span body.span)
    | do_kw :: body_rest when token_kind KwDo do_kw ->
        let body_terms, rest, span = collect_until_end do_kw.span body_rest in
        let body = parse_do_body_terms span body_terms in
        (body, rest, span_between start_span span)
    | _ -> error "expected -> or do after fn parameters"
  in
  (List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body, rest)

and parse_if start_span terms =
  match split_at_token KwDo terms with
  | None -> unsupported "old if syntax is handled by the compatibility parser"
  | Some (cond_terms, do_kw, rest) ->
      let cond = parse_all (fun ts -> parse_expr_prec 0 ts) cond_terms in
      let then_terms, else_terms, rest, end_span = collect_if_branches do_kw.span rest in
      let then_ = parse_do_body_terms (syntax_span then_terms) then_terms in
      let else_ = parse_do_body_terms (syntax_span else_terms) else_terms in
      (stx ~span:(span_between start_span end_span) (Syntax.If { cond; then_; else_ }), rest)

and collect_if_branches _do_span terms =
  let rec collect_then depth acc = function
    | [] -> error "unterminated if block"
    | term :: rest when token_kind KwDo term -> collect_then (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then error "if block requires else"
        else collect_then (depth - 1) (term :: acc) rest
    | term :: rest when token_kind KwElse term && depth = 0 ->
        let then_terms = List.rev acc in
        let else_terms, rest, end_span = collect_else 0 [] rest in
        (then_terms, else_terms, rest, end_span)
    | term :: rest -> collect_then depth (term :: acc) rest
  and collect_else depth acc = function
    | [] -> error "unterminated else block"
    | term :: rest when token_kind KwDo term -> collect_else (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, term.span)
        else collect_else (depth - 1) (term :: acc) rest
    | term :: rest -> collect_else depth (term :: acc) rest
  in
  collect_then 0 [] terms

and parse_pat_terms terms =
  let rec parse_pat_atom terms =
    match drop_separators terms with
    | [] -> error "expected pattern"
    | term :: rest -> (
        match term.datum with
        | Token { kind = Int n; _ } -> (Syntax.PatAtom (Atom.I64 n), rest)
        | Token { kind = Char c; _ } -> (Syntax.PatAtom (Atom.Char c), rest)
        | Token { kind = Unit; _ } -> (Syntax.PatAtom Atom.Unit, rest)
        | Token { kind = KwTrue; _ } -> (Syntax.PatAtom (Atom.Bool true), rest)
        | Token { kind = KwFalse; _ } -> (Syntax.PatAtom (Atom.Bool false), rest)
        | Token { kind = Ident name; _ } ->
            let pat =
              match name with
              | "_" -> Syntax.PatWild
              | "I64" -> Syntax.PatType Atom_ty.TI64
              | "Bool" -> Syntax.PatType Atom_ty.TBool
              | "Unit" -> Syntax.PatType Atom_ty.TUnit
              | "Char" -> Syntax.PatType Atom_ty.TChar
              | "String" -> Syntax.PatType Atom_ty.TString
              | "Absurd" -> Syntax.PatType Atom_ty.TAbsurd
              | _ when name <> "" && Char.uppercase_ascii name.[0] = name.[0] ->
                  Syntax.PatCon ([], name, [])
              | _ -> Syntax.PatBind (id ~span:term.span name)
            in
            (pat, rest)
        | Group (Raw_syntax.Paren, items, _) -> (
            match split_commas items with
            | [ only ] -> (parse_pat_all only, rest)
            | parts -> (Syntax.PatProd (List.map parse_pat_all parts), rest))
        | _ -> error "unsupported pattern in Phase 7B match")
  and parse_pat_postfix lhs terms =
    match drop_separators terms with
    | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
        let args =
          match drop_separators items with
          | [] -> []
          | _ -> List.map parse_pat_all (split_commas items)
        in
        let lhs =
          match lhs with
          | Syntax.PatCon (path, name, []) -> Syntax.PatCon (path, name, args)
          | _ -> error "only constructor patterns can take arguments"
        in
        parse_pat_postfix lhs rest
    | rest -> (lhs, rest)
  and parse_pat_or terms =
    match split_at_token Bar terms with
    | Some (lhs_terms, _, rhs_terms) -> Syntax.PatOr (parse_pat_all lhs_terms, parse_pat_all rhs_terms)
    | None ->
        let lhs, rest = parse_pat_atom terms in
        let lhs, rest = parse_pat_postfix lhs rest in
        let rest = drop_separators rest in
        if rest = [] then lhs else error "unconsumed terms after pattern"
  and parse_pat_all terms = parse_pat_or terms in
  parse_pat_all terms

and parse_match start_span terms =
  match split_at_token KwDo terms with
  | None -> unsupported "old match syntax is handled by the compatibility parser"
  | Some (scrut_terms, do_kw, rest) ->
      let scrut = parse_all (fun ts -> parse_expr_prec 0 ts) scrut_terms in
      let body_terms, rest, span = collect_until_end do_kw.span rest in
      let branches =
        split_by_top_level_bar body_terms
        |> List.map (fun branch_terms ->
               match split_at_arrow branch_terms with
               | Some (pat_terms, _, body_terms) ->
                   let pat = parse_pat_terms pat_terms in
                   let body = parse_all (fun ts -> parse_expr_prec 0 ts) body_terms in
                   Syntax.ValueBranch (pat, body)
               | None -> error "match branch requires ->")
      in
      if branches = [] then error "match requires at least one branch";
      (stx ~span:(span_between start_span span) (Syntax.Match (scrut, branches)), rest)

and parse_group_arg items =
  match drop_separators items with
  | [] -> unit ()
  | _ -> parse_all (fun ts -> parse_expr_prec 0 ts) items

and parse_ref start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg items in
      (stx ~span:(span_between start_span span) (Syntax.RefNew arg), rest)
  | _ -> unsupported "old ref syntax is handled by the compatibility parser"

and parse_deref start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg items in
      (stx ~span:(span_between start_span span) (Syntax.RefGet arg), rest)
  | _ -> error "deref requires a parenthesized argument"

and parse_resume start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg items in
      (stx ~span:(span_between start_span span) (Syntax.Resume arg), rest)
  | _ -> unsupported "old resume syntax is handled by the compatibility parser"

and parse_import start_span terms =
  match drop_separators terms with
  | { datum = Token { kind = String path; _ }; span } :: rest ->
      (stx ~span:(span_between start_span span) (Syntax.Import path), rest)
  | _ -> error "import requires a string path"

and parse_primary terms =
  match drop_separators terms with
  | [] -> error "expected expression"
  | term :: rest -> (
      match term.datum with
      | Token { kind = Int n; _ } -> (atom ~span:term.span (Atom.I64 n), rest)
      | Token { kind = String s; _ } -> (atom ~span:term.span (Atom.String s), rest)
      | Token { kind = Char c; _ } -> (atom ~span:term.span (Atom.Char c), rest)
      | Token { kind = Unit; _ } -> (unit ~span:term.span (), rest)
      | Token { kind = KwTrue; _ } -> (atom ~span:term.span (Atom.Bool true), rest)
      | Token { kind = KwFalse; _ } -> (atom ~span:term.span (Atom.Bool false), rest)
      | Token { kind = KwUnit; _ } -> (var ~span:term.span "Unit", rest)
      | Token { kind = KwSelf; _ } -> (stx ~span:term.span Syntax.Self, rest)
      | Token { kind = KwSelfType; _ } -> (stx ~span:term.span Syntax.SelfType, rest)
      | Token { kind = KwDo; _ } -> parse_do term.span rest
      | Token { kind = KwFn; _ } -> parse_fn term.span rest
      | Token { kind = KwIf; _ } -> parse_if term.span rest
      | Token { kind = KwMatch; _ } -> parse_match term.span rest
      | Token { kind = KwRef; _ } -> parse_ref term.span rest
      | Token { kind = KwDeref; _ } -> parse_deref term.span rest
      | Token { kind = KwResume; _ } -> parse_resume term.span rest
      | Token { kind = KwImport; _ } -> parse_import term.span rest
      | Token { kind = Ident name; _ } -> (
          match Operator_env.find_prefix name with
          | Some op ->
              let rhs, rest = parse_expr_prec op.precedence rest in
              let span = span_between term.span rhs.span in
              (ap ~span (var ~span:term.span name) Explicitness.Explicit rhs, rest)
          | None -> (var ~span:term.span name, rest))
      | Token { kind = Operator name; _ } -> (
          match Operator_env.find_prefix name with
          | Some op ->
              let rhs, rest = parse_expr_prec op.precedence rest in
              let span = span_between term.span rhs.span in
              (ap ~span (var ~span:term.span name) Explicitness.Explicit rhs, rest)
          | None -> unsupported ("unsupported prefix operator: " ^ name))
      | Token { kind = Eof; _ } -> error "unexpected EOF in expression"
      | Token { kind; _ } -> (
          match keyword_name kind with
          | Some name -> unsupported ("unsupported Phase 7A keyword: " ^ name)
          | None -> (
              match punct_name kind with
              | Some name -> error ("unexpected punctuation in expression: " ^ name)
              | None -> error "unexpected token in expression"))
      | Group (delimiter, items, span) -> (parse_group_expr delimiter items span, rest))

and parse_expr_prec min_prec terms =
  let lhs, rest = parse_primary terms in
  parse_postfix_infix min_prec lhs rest

and parse_postfix_infix min_prec lhs terms =
  match terms with
  | term :: _ when is_separator term -> (lhs, terms)
  | ({ datum = Group (Raw_syntax.Paren, items, span); _ } as term) :: rest ->
      let args =
        match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args items
      in
      let call_span = span_between lhs.span term.span in
      let lhs = List.fold_left (fun f arg -> ap ~span:call_span f Explicitness.Explicit arg) lhs args in
      parse_postfix_infix min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Bracket, items, _); _ } as term) :: rest ->
      let args = parse_args items in
      let call_span = span_between lhs.span term.span in
      let lhs = List.fold_left (fun f arg -> ap ~span:call_span f Explicitness.Implicit arg) lhs args in
      parse_postfix_infix min_prec lhs rest
  | term :: field :: rest when token_kind Dot term -> (
      match token_text field with
      | Some name ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.FieldAccess (lhs, name)) in
          parse_postfix_infix min_prec lhs rest
      | None -> unsupported "numeric projection after '.' is handled by the compatibility parser")
  | term :: rest -> (
      match token_text term with
      | Some symbol -> (
          match Operator_env.find_infix symbol with
          | Some op when op.precedence >= min_prec ->
              let next_min =
                match op.associativity with
                | Left -> op.precedence + 1
                | Right -> op.precedence
              in
              let rhs, rest = parse_expr_prec next_min rest in
              let span = span_between lhs.span rhs.span in
              let lhs =
                if String.equal op.symbol "<-" then stx ~span (Syntax.RefSet (lhs, rhs))
                else
                  ap ~span
                    (ap ~span (var ~span:term.span op.symbol) Explicitness.Explicit lhs)
                    Explicitness.Explicit rhs
              in
              parse_postfix_infix min_prec lhs rest
          | _ -> (lhs, term :: rest))
      | None -> (lhs, term :: rest))
  | [] -> (lhs, [])

and collect_do_body start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated do block"
    | term :: rest when token_kind KwDo term -> go (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, span_between start_span term.span)
        else go (depth - 1) (term :: acc) rest
    | term :: rest -> go depth (term :: acc) rest
  in
  go 0 [] terms

and split_statements terms =
  split_commas terms
  |> List.concat_map (fun comma_part ->
         let rec go current acc = function
           | [] -> List.rev (List.rev current :: acc)
           | term :: rest when is_separator term -> go [] (List.rev current :: acc) rest
           | term :: rest -> go (term :: current) acc rest
         in
         go [] [] comma_part)
  |> List.filter (fun stmt -> not (List.for_all is_separator stmt || stmt = []))

and parse_binding_statement stmt =
  match stmt with
  | [ { datum = Token { kind = Ident name; _ }; span = name_span }; eq ] when token_kind Equals eq ->
      error ("missing value for binding: " ^ name ^ " at " ^ Format.asprintf "%a" Source_span.pp name_span)
  | { datum = Token { kind = Ident name; _ }; span = name_span } :: eq :: value_terms
    when token_kind Equals eq ->
      let value = parse_all (fun ts -> parse_expr_prec 0 ts) value_terms in
      Some (id ~span:name_span name, value)
  | _ -> None

and parse_open_statement stmt =
  match drop_separators stmt with
  | [ { datum = Token { kind = KwOpen; _ }; _ }; { datum = Token { kind = Ident name; _ }; span } ] ->
      Some (id ~span name)
  | _ -> None

and parse_do_body_terms span body_terms =
  let statements = split_statements body_terms in
  match List.rev statements with
  | [] -> error "empty do block"
  | final_stmt :: rev_statements ->
      let body = parse_all (fun ts -> parse_expr_prec 0 ts) final_stmt in
      let statements = List.rev rev_statements in
      List.fold_right
        (fun stmt acc ->
          match parse_binding_statement stmt with
          | Some (name, value) ->
              stx ~span
                (Syntax.Let { name; type_ = None; value; body = acc; recursive = false })
          | None -> (
              match parse_open_statement stmt with
              | Some name -> stx ~span (Syntax.Open (name, acc))
              | None ->
                  error
                    "only simple name = expr and open M statements are supported before the final expression in Phase 7B do blocks"))
        statements body

and parse_do start_span terms =
  let body_terms, rest, span = collect_do_body start_span terms in
  (parse_do_body_terms span body_terms, rest)

let parse_terms terms = parse_all (fun ts -> parse_expr_prec 0 ts) terms

let parse_expr ?file source =
  try Raw_syntax.read ?file source |> parse_terms with
  | Raw_syntax.Error msg -> error msg
