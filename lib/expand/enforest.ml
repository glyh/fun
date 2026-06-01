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
      | Token { kind = KwDo; _ } -> parse_do term.span rest
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

and parse_do start_span terms =
  let body_terms, rest, span = collect_do_body start_span terms in
  let statements = split_statements body_terms in
  match List.rev statements with
  | [] -> error "empty do block"
  | final_stmt :: rev_bindings ->
      let body = parse_all (fun ts -> parse_expr_prec 0 ts) final_stmt in
      let bindings = List.rev rev_bindings in
      let body =
        List.fold_right
          (fun stmt acc ->
            match parse_binding_statement stmt with
            | Some (name, value) ->
                stx ~span
                  (Syntax.Let { name; type_ = None; value; body = acc; recursive = false })
            | None -> error "only simple name = expr statements are supported before the final expression in Phase 7A do blocks")
          bindings body
      in
      (body, rest)

let parse_terms terms = parse_all (fun ts -> parse_expr_prec 0 ts) terms

let parse_expr ?file source =
  try Raw_syntax.read ?file source |> parse_terms with
  | Raw_syntax.Error msg -> error msg
