exception Unsupported of string
exception Error of string

type value_decl = {
  decl_name : Syntax.id;
  decl_type : Syntax.t option;
  decl_value : Syntax.t;
  decl_recursive : bool;
}

type syntax_decl =
  | MacroSyntaxDecl of {
      syntax_name : Syntax.id;
      syntax_value : Syntax.t;
      syntax_export : Operator_env.export;
    }
  | TemplateSyntaxDecl of {
      syntax_name : Syntax.id;
      syntax_export : Operator_env.export;
    }

type env = {
  mutable operators : Operator_env.t;
  mutable template_captures : (string * Syntax_template.captured) list;
  exports_collector : Operator_env.export list ref;
  load_syntax : (string -> Operator_env.export list) option;
  syntax_class : Syntax_class.t;
}

let env ?load_syntax ?(syntax_class = Syntax_class.Expr) () =
  { operators = Operator_env.empty; template_captures = []; load_syntax; syntax_class; exports_collector = ref [] }

let unsupported msg = raise (Unsupported msg)
let error msg = raise (Error msg)

let rec first_some parsers input =
  match parsers with
  | [] -> None
  | parser :: rest -> (
      match parser input with
      | Some value -> Some value
      | None -> first_some rest input)

open Raw_syntax

let id ?span name =
  match span with
  | Some span -> Syntax.fresh_id ~span name
  | None -> Syntax.fresh_id name

let stx ?(span = Source_span.synthetic) kind = { Syntax.kind; span }

let atom ?span atom = stx ?span (Syntax.Atom atom)
let var ?span name = stx ?span (Syntax.Var (id ?span name))

let syntax_operator_arg ~span ~use_span (op : Operator_env.operator) operands =
  let fixity = match op.fixity with Operator_env.Prefix -> Syntax.PrefixOp | Operator_env.Infix -> Syntax.InfixOp in
  stx ~span
    (Syntax.SyntaxOperatorUse
       { operator = id ~span:use_span op.symbol;
         fixity;
         operands;
         declaration_span = op.declaration_span;
         use_span })

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
  | Token { kind = KwDeref; _ } -> Some "deref"
  | Token { kind = KwRef; _ } -> Some "ref"
  | Token { kind = KwModule; _ } -> Some "module"
  | Token { kind = KwStruct; _ } -> Some "struct"
  | Token { kind = KwType; _ } -> Some "type"
  | Token { kind = KwEffect; _ } -> Some "effect"
  | Token { kind = KwTrait; _ } -> Some "trait"
  | Token { kind = KwImpl; _ } -> Some "impl"
  | _ -> None

let token_kind kind term =
  match term.datum with
  | Token { kind = k; _ } -> k = kind
  | _ -> false

let keyword_name = function
  | KwFn -> Some "fn"
  | KwEnd -> Some "end"
  | KwLet -> Some "let"
  | KwFun -> Some "fun"
  | KwThen -> Some "then"
  | KwSig -> Some "sig"
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

let is_expr_start env term =
  match term.datum with
  | Token { kind = Int _ | Char _ | String _ | Unit | KwTrue | KwFalse | KwUnit | KwSelf | KwSelfType | KwDo | KwFn | KwIf | KwMatch | KwRef | KwDeref | KwResume | KwImport | KwModule | KwSig | KwStruct | KwMacro | KwType | KwEffect | KwTrait | KwImpl | Ident _; _ } -> true
  | Token { kind = Operator s; _ } -> Option.is_some (Operator_env.find_prefix ~syntax_class:env.syntax_class env.operators s)
  | Group (Raw_syntax.Paren, _, _) -> true
  | _ -> false

let is_adjacent_postfix (lhs : Syntax.t) (term : Raw_syntax.t) =
  lhs.span.synthetic || term.span.synthetic || lhs.span.end_byte = term.span.start_byte

let spans_adjacent (lhs : Source_span.t) (rhs : Source_span.t) =
  lhs.synthetic || rhs.synthetic || lhs.end_byte = rhs.start_byte

let require_adjacent_postfix lhs term what =
  if not (is_adjacent_postfix lhs term) then
    error (what ^ " must be adjacent to the callee; whitespace application is not supported")

let require_adjacent_span lhs rhs what =
  if not (spans_adjacent lhs rhs) then
    error (what ^ " must be adjacent; whitespace form is not supported")

let split_last_expr terms =
  let rec go acc = function
    | [] -> error "expected trailing expression"
    | [ last ] -> (List.rev acc, [ last ])
    | term :: rest -> go (term :: acc) rest
  in
  go [] (drop_separators terms)

let dotted_id_from_terms terms =
  let rec go path = function
    | { datum = Token { kind = Ident name; _ }; _ } :: dot :: rest when token_kind Dot dot ->
        go (path @ [ name ]) rest
    | { datum = Token { kind = Ident name; _ }; _ } :: rest -> ((path, name), rest)
    | _ -> error "expected dotted identifier"
  in
  go [] (drop_separators terms)

let binding_name_term = function
  | { datum = Token { kind = Ident name; _ }; span } -> Some (id ~span name)
  | { datum = Group (Raw_syntax.Paren, [ { datum = Token { kind = Operator name; _ }; span } ], _); _ } ->
      Some (id ~span name)
  | { datum = Group (Raw_syntax.Paren, [ { datum = Token { kind = Ident name; _ }; span } ], _); _ } ->
      Some (id ~span name)
  | _ -> None

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
  let first_unconsumed_name term =
    match term.datum with
    | Group (Paren, _, _) -> "("
    | Group (Bracket, _, _) -> "["
    | Group (Brace, _, _) -> "{"
    | Token _ -> (
        match token_text term with
        | Some text -> text
        | None -> (
            match term.datum with
            | Token { kind; _ } ->
                Option.value ~default:"<syntax>" (punct_name kind)
            | Group _ -> "<syntax>"))
  in
  let terms = drop_separators terms in
  match terms with
  | [] -> error "expected expression"
  | _ ->
      let expr, rest = parse terms in
      let rest = drop_separators rest in
      if rest = [] then expr
      else
        let first =
          match rest with
          | term :: _ -> first_unconsumed_name term
          | [] -> "<none>"
        in
        unsupported ("unconsumed terms after expression: " ^ first)

let rec split_at_pred pred acc = function
  | [] -> None
  | term :: rest when pred term -> Some (List.rev acc, term, rest)
  | term :: rest -> split_at_pred pred (term :: acc) rest

let split_at_token kind terms = split_at_pred (token_kind kind) [] terms

let split_at_arrow terms = split_at_token ThinArrow terms

let starts_named_do_block terms =
  match drop_separators terms with
  | { datum = Token { kind = Ident _; _ }; _ } :: do_kw :: _ when token_kind KwDo do_kw -> true
  | _ -> false

let is_multi_block_start term =
  match term.datum with
  | Token { kind = Ident "multi"; _ } -> true
  | _ -> false

let split_by_top_level_bar terms =
  let rec go depth current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when depth = 0 && token_kind Bar term -> go depth [] (List.rev current :: acc) rest
    | term :: rest when token_kind KwDo term || token_kind KwSig term -> go (depth + 1) (term :: current) acc rest
    | term :: rest when token_kind KwModule term ->
        if starts_named_do_block rest then go depth (term :: current) acc rest
        else go (depth + 1) (term :: current) acc rest
    | term :: rest when token_kind KwStruct term ->
        if starts_named_do_block rest then go depth (term :: current) acc rest
        else go (depth + 1) (term :: current) acc rest
    | term :: rest when token_kind KwEnd term && depth > 0 -> go (depth - 1) (term :: current) acc rest
    | term :: rest -> go depth (term :: current) acc rest
  in
  go 0 [] [] terms |> List.filter (fun part -> drop_separators part <> [])

let split_match_branches terms =
  let rec go depth seen_arrow current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when depth = 0 && token_kind Bar term && drop_separators current = [] ->
        go depth false current acc rest
    | term :: rest when depth = 0 && token_kind Bar term && seen_arrow ->
        go depth false [] (List.rev current :: acc) rest
    | term :: rest when depth = 0 && token_kind ThinArrow term ->
        go depth true (term :: current) acc rest
    | term :: rest when token_kind KwDo term || token_kind KwSig term || is_multi_block_start term ->
        go (depth + 1) seen_arrow (term :: current) acc rest
    | term :: rest when token_kind KwModule term ->
        if starts_named_do_block rest then go depth seen_arrow (term :: current) acc rest
        else go (depth + 1) seen_arrow (term :: current) acc rest
    | term :: rest when token_kind KwStruct term ->
        if starts_named_do_block rest then go depth seen_arrow (term :: current) acc rest
        else go (depth + 1) seen_arrow (term :: current) acc rest
    | term :: rest when token_kind KwEnd term && depth > 0 ->
        go (depth - 1) seen_arrow (term :: current) acc rest
    | term :: rest -> go depth seen_arrow (term :: current) acc rest
  in
  go 0 false [] [] terms |> List.filter (fun part -> drop_separators part <> [])

let collect_until_end start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated block"
    | term :: rest
      when token_kind KwDo term || token_kind KwSig term || is_multi_block_start term
           || ((token_kind KwStruct term || token_kind KwModule term) && not (starts_named_do_block rest)) ->
        go (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, span_between start_span term.span)
        else go (depth - 1) (term :: acc) rest
    | term :: rest -> go depth (term :: acc) rest
  in
  go 0 [] terms

let collect_match_until_end start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated match block"
    | term :: rest
      when token_kind KwDo term || token_kind KwStruct term || token_kind KwModule term
           || token_kind KwSig term || token_kind KwMatch term || token_kind KwTrait term
           || token_kind KwImpl term ->
        go (depth + 1) (term :: acc) rest
    | term :: rest when token_kind KwEnd term ->
        if depth = 0 then (List.rev acc, rest, span_between start_span term.span)
        else go (depth - 1) (term :: acc) rest
    | term :: rest -> go depth (term :: acc) rest
  in
  go 0 [] terms

let ensure_no_rest what rest =
  match drop_separators rest with
  | [] -> ()
  | _ -> error (what ^ " has trailing terms")

let with_operator_scope env f =
  f { env with operators = env.operators; template_captures = env.template_captures }

let syntax_name term = token_text term

let required_syntax_name term what =
  match syntax_name term with Some name -> name | None -> error (what ^ " requires an operator or identifier name")

let load_syntax_exports env path =
  match env.load_syntax with
  | None -> ()
  | Some load ->
      let exports = load path in
      (match Operator_env.duplicate_exports_message exports with Some msg -> error msg | None -> ());
      env.operators <- Operator_env.apply_exports env.operators exports

let rec load_imports_in_terms env = function
  | { datum = Token { kind = KwImport; _ }; _ }
    :: { datum = Token { kind = String path; _ }; _ } :: rest ->
      load_syntax_exports env path;
      load_imports_in_terms env rest
  | { datum = Group (_, items, _); _ } :: rest ->
      load_imports_in_terms env items;
      load_imports_in_terms env rest
  | _ :: rest -> load_imports_in_terms env rest
  | [] -> ()


and split_statements terms =
  let is_statement_separator depth term =
    depth = 0 && (is_separator term || token_kind Comma term)
  in
  let rec go depth current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when is_statement_separator depth term -> go depth [] (List.rev current :: acc) rest
    | term :: rest when token_kind KwDo term || token_kind KwSig term || is_multi_block_start term ->
        go (depth + 1) (term :: current) acc rest
    | term :: rest when token_kind KwModule term ->
        if starts_named_do_block rest then go depth (term :: current) acc rest
        else go (depth + 1) (term :: current) acc rest
    | term :: rest when token_kind KwStruct term -> (
        if starts_named_do_block rest then go depth (term :: current) acc rest
        else
          match drop_separators rest with
          | next :: _ when token_kind KwDo next -> go depth (term :: current) acc rest
          | _ -> go (depth + 1) (term :: current) acc rest)
    | term :: rest when token_kind KwEnd term && depth > 0 -> go (depth - 1) (term :: current) acc rest
    | term :: rest -> go depth (term :: current) acc rest
  in
  let stmts = go 0 [] [] terms in
  let stmts = List.filter (fun stmt -> not (List.for_all is_separator stmt || stmt = [])) stmts in
  let rec merge_cont (acc : Raw_syntax.t list list) (stmts : Raw_syntax.t list list) =
    match stmts with
    | [] -> List.rev acc
    | [ last ] -> List.rev (last :: acc)
    | a :: b :: rest ->
        let a_ends_bar = match List.rev a with
          | (term : Raw_syntax.t) :: _ when token_kind Bar term -> true
          | _ -> false
        in
        let b_starts_bar = match drop_separators b with
          | (term : Raw_syntax.t) :: _ when token_kind Bar term -> true
          | _ -> false
        in
        if a_ends_bar || b_starts_bar then
          merge_cont acc ((a @ b) :: rest)
        else
          merge_cont (a :: acc) (b :: rest) in
  merge_cont [] stmts
