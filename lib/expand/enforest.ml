exception Unsupported of string
exception Error of string

type value_decl = {
  decl_name : Syntax.id;
  decl_type : Syntax.t option;
  decl_value : Syntax.t;
  decl_recursive : bool;
}

type syntax_decl = {
  syntax_name : Syntax.id;
  syntax_value : Syntax.t;
  syntax_export : Operator_env.export;
}

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

let is_expr_start term =
  match term.datum with
  | Token { kind = Int _ | Char _ | String _ | Unit | KwTrue | KwFalse | KwUnit | KwSelf | KwSelfType | KwDo | KwFn | KwIf | KwMatch | KwRef | KwDeref | KwResume | KwImport | KwModule | KwSig | KwStruct | KwMacro | KwType | KwEffect | KwTrait | KwImpl | Ident _; _ } -> true
  | Token { kind = Operator s; _ } -> Option.is_some (Operator_env.find_prefix s)
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

let starts_named_do_block terms =
  match drop_separators terms with
  | { datum = Token { kind = Ident _; _ }; _ } :: do_kw :: _ when token_kind KwDo do_kw -> true
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
    | term :: rest when token_kind KwDo term || token_kind KwSig term ->
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
      when token_kind KwDo term || token_kind KwSig term
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

let with_operator_scope f =
  let snapshot = Operator_env.snapshot () in
  Fun.protect ~finally:(fun () -> Operator_env.restore snapshot) f

let syntax_name term = token_text term

let current_load_syntax : (string -> Operator_env.export list) option ref = ref None

let with_syntax_loader load_syntax f =
  let previous = !current_load_syntax in
  current_load_syntax := load_syntax;
  Fun.protect ~finally:(fun () -> current_load_syntax := previous) f

let load_syntax_exports path =
  match !current_load_syntax with
  | None -> ()
  | Some load -> Operator_env.apply_exports (load path)

let rec load_imports_in_terms = function
  | { datum = Token { kind = KwImport; _ }; _ }
    :: { datum = Token { kind = String path; _ }; _ } :: rest ->
      load_syntax_exports path;
      load_imports_in_terms rest
  | { datum = Group (_, items, _); _ } :: rest ->
      load_imports_in_terms items;
      load_imports_in_terms rest
  | _ :: rest -> load_imports_in_terms rest
  | [] -> ()

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
      | [ { datum = Token { kind = Operator name; _ }; _ } ] -> var ~span name
      | _ -> (
          match split_at_token Colon items with
          | Some (expr_terms, _, typ_terms) ->
              stx ~span (Syntax.Annotated { inner = parse_all (fun ts -> parse_expr_prec 0 ts) expr_terms; typ = parse_type_terms typ_terms })
          | None ->
          match split_commas items with
          | [ only ] -> { (parse_all (fun ts -> parse_expr_prec 0 ts) only) with span }
          | parts ->
              let exprs = List.map (parse_all (fun ts -> parse_expr_prec 0 ts)) parts in
              stx ~span (Syntax.Prod exprs)))
  | Bracket -> unsupported "bare bracket expression is not in Phase 7A"
  | Brace -> unsupported "bare brace expression is not in Phase 7A"

and parse_record_expr_fields items =
  split_statements items
  |> List.map (fun part ->
         match drop_separators part with
         | { datum = Token { kind = Ident name; _ }; _ } :: eq :: value_terms when token_kind Equals eq ->
             (name, parse_all (fun ts -> parse_expr_prec 0 ts) value_terms)
         | _ -> error "expected record field of the form name = expr")

and parse_type_terms terms = parse_all parse_type_entry terms

and parse_type_entry (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list = parse_type_arrow terms

and parse_type_arrow (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_plus terms in
  match drop_separators rest with
  | { datum = Token { kind = ThinArrow; _ }; _ } :: rest ->
      let cod, rest = parse_type_arrow (drop_separators rest) in
      let lhs = stx ~span:(span_between lhs.span cod.span) (Syntax.Arrow (Explicitness.Explicit, None, lhs, None, cod)) in
      parse_type_arrow_effect lhs rest
  | rest -> parse_type_arrow_effect lhs rest

and parse_type_arrow_effect (lhs : Syntax.t) (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = KwCan; _ }; _ } :: rest -> (
      match lhs.kind with
      | Syntax.Arrow _ ->
          let eff, rest = parse_can_effect_row rest in
          parse_type_arrow_effect (attach_effects lhs eff) rest
      | _ -> (lhs, terms))
  | terms -> (lhs, terms)

and parse_type_plus (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_product terms in
  parse_type_plus_tail lhs rest

and parse_type_plus_tail (lhs : Syntax.t) (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = Operator "+"; _ }; span = op_span } :: rest ->
      let rhs, rest = parse_type_product rest in
      let span = span_between lhs.span rhs.span in
      let lhs = ap ~span (ap ~span (var ~span:op_span "+") Explicitness.Explicit lhs) Explicitness.Explicit rhs in
      parse_type_plus_tail lhs rest
  | rest -> (lhs, rest)

and parse_type_product (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  let lhs, rest = parse_type_atom terms in
  let lhs, rest = parse_type_postfix lhs rest in
  parse_type_product_tail lhs rest

and parse_type_product_tail (lhs : Syntax.t) (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | { datum = Token { kind = Operator "*"; _ }; _ } :: rest ->
      let rhs, rest = parse_type_atom rest in
      let rhs, rest = parse_type_postfix rhs rest in
      let elems =
        (match lhs.kind with Syntax.ProdTy xs -> xs | _ -> [ lhs ])
        @ (match rhs.kind with Syntax.ProdTy xs -> xs | _ -> [ rhs ])
      in
      parse_type_product_tail (stx ~span:(span_between lhs.span rhs.span) (Syntax.ProdTy elems)) rest
  | rest -> (lhs, rest)

and parse_type_atom (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  match drop_separators terms with
  | [] -> error "expected type"
  | { datum = Token { kind = Ident name; _ }; span } :: rest ->
      (var ~span name, rest)
  | { datum = Token { kind = KwUnit; _ }; span } :: rest ->
      (var ~span "Unit", rest)
  | { datum = Token { kind = KwSelfType; _ }; span } :: rest ->
      (stx ~span Syntax.SelfType, rest)
  | { datum = Token { kind = KwFn; _ }; span } :: rest -> parse_fn span rest
  | { datum = Token { kind = KwModule; _ }; span } :: rest -> parse_module_expr span rest
  | { datum = Token { kind = KwSig; _ }; span } :: rest -> parse_sig_expr span rest
  | { datum = Token { kind = KwStruct; _ }; span } :: rest -> parse_struct_expr span rest
  | { datum = Token { kind; _ }; _ } :: _ ->
      let name = keyword_name kind in
      if Option.is_some name then
        let name = Option.get name in
        if String.equal name "ref" || String.equal name "EffectRow" ||
           String.equal name "Type" ||
           String.equal name "I64" || String.equal name "Bool" ||
           String.equal name "Unit" || String.equal name "Char" ||
           String.equal name "String" || String.equal name "Absurd"
        then
          (match terms with
           | { span; _ } :: rest -> (var ~span name, rest)
           | _ -> error "expected type token")
        else unsupported ("unexpected keyword in type: " ^ name)
      else error "expected type atom"
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: arrow :: rest
    when token_kind ThinArrow arrow && List.exists (token_kind Colon) items ->
      let params = parse_param_group Explicitness.Explicit items in
      let (cod : Syntax.t), rest = parse_type_arrow rest in
      let (result : Syntax.t) =
        List.fold_right
          (fun (p : Syntax.param) (acc : Syntax.t) ->
             let dom = Option.value ~default:(unit_type ()) p.type_ in
             stx ~span:(span_between span acc.span)
               (Syntax.Arrow (Explicitness.Explicit, Some p.name, dom, None, acc)))
          params cod
      in
      (result, rest)
  | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest ->
      let items = drop_separators items in
      let ty =
        match split_commas items with
        | [ only ] -> parse_all parse_type_entry only
        | _ -> error "tuple/product types use * syntax, not comma syntax"
      in
      (ty, rest)
  | { datum = Group (Raw_syntax.Bracket, items, _); _ } :: rest ->
      let (params : Syntax.param list) = parse_param_group Explicitness.Implicit items in
      let cod_items, rest =
        match drop_separators rest with
        | arrow :: cod_terms when token_kind ThinArrow arrow -> (cod_terms, [])
        | _ -> error "expected -> after implicit Pi parameters"
      in
      let cod = parse_all parse_type_entry cod_items in
      let result =
        List.fold_right
          (fun (p : Syntax.param) acc ->
             let dom = Option.value ~default:(unit_type ()) p.type_ in
             stx ~span:(span_between p.name.span cod.span)
               (Syntax.Arrow (Explicitness.Implicit, Some p.name, dom, None, acc)))
          params cod
      in
      (result, rest)
  | { datum = Group (Raw_syntax.Brace, _, _); _ } :: _ ->
      unsupported "bare brace type syntax is only supported in effect rows after can"

and parse_type_postfix (lhs : Syntax.t) (terms : Raw_syntax.t list) : Syntax.t * Raw_syntax.t list =
  match terms with
  | { datum = Group (Raw_syntax.Paren, items, _); span = group_span } :: rest ->
      let args = parse_args items in
      let call_span = span_between lhs.span group_span in
      let lhs = List.fold_left (fun f arg -> ap ~span:call_span f Explicitness.Explicit arg) lhs args in
      parse_type_postfix lhs rest
  | dot :: field :: rest when token_kind Dot dot -> (
      match token_text field with
      | Some name ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.FieldAccess (lhs, name)) in
          parse_type_postfix lhs rest
      | None -> unsupported "numeric projection in type is handled by the compatibility parser")
  | _ -> (lhs, terms)

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

and parse_fn_parts ?(allow_empty = false) start_span terms =
  let terms = drop_separators terms in
  let implicit_params, rest =
    match terms with
    | ({ datum = Group (Raw_syntax.Bracket, items, _); _ } as group) :: rest ->
        require_adjacent_span start_span group.span "implicit fn parameter list";
        (parse_param_group Explicitness.Implicit items, rest)
    | rest -> ([], rest)
  in
  let explicit_params, rest =
    match drop_separators rest with
    | ({ datum = Group (Raw_syntax.Paren, items, _); _ } as group) :: rest ->
        let previous_span =
          match terms with
          | ({ datum = Group (Raw_syntax.Bracket, _, _); _ } as implicit_group) :: _ -> implicit_group.span
          | _ -> start_span
        in
        require_adjacent_span previous_span group.span "explicit fn parameter list";
        (parse_param_group Explicitness.Explicit items, rest)
    | rest when implicit_params <> [] || allow_empty -> ([], rest)
    | _ -> error "fn requires at least one parameter list"
  in
  let params = implicit_params @ explicit_params in
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
  (params, body, rest, span)

and parse_fn start_span terms =
  let params, body, rest, span = parse_fn_parts start_span terms in
  (List.fold_right (fun p acc -> stx ~span (Syntax.Lam (p, acc))) params body, rest)

and parse_method_params items =
  let items = drop_separators items in
  if items = [] then [] else parse_param_group Explicitness.Explicit items

and parse_method_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwMethod; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
       :: ({ datum = Group (Raw_syntax.Paren, items, _); _ } as params_group)
          :: rest ->
      require_adjacent_span name_span params_group.span "method parameter list";
      let params = parse_method_params items in
      let body, rest =
        match drop_separators rest with
        | arrow :: body_terms when token_kind ThinArrow arrow ->
            (parse_all (fun ts -> parse_expr_prec 0 ts) body_terms, [])
        | do_kw :: body_rest when token_kind KwDo do_kw ->
            let body_terms, rest, span = collect_until_end do_kw.span body_rest in
            (parse_do_body_terms span body_terms, rest)
        | _ -> error "expected -> or do after method parameters"
      in
      ensure_no_rest "method declaration" rest;
      Some (Syntax.MethodBinding { name = id ~span:name_span name; params; body; public })
  | { datum = Token { kind = KwMethod; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; _ } :: _ ->
      error ("method declaration requires a parenthesized parameter list: " ^ name)
  | _ -> None

and parse_module_type_fields what terms =
  let module_syntax, body_terms =
    match drop_separators terms with
    | { datum = Token { kind = KwSig; _ }; span } :: rest ->
        let body_terms, rest, _span = collect_until_end span rest in
        ensure_no_rest what rest;
        (false, body_terms)
    | { datum = Token { kind = KwModule; _ }; span } :: rest ->
        let body_terms, rest, _span = collect_until_end span rest in
        ensure_no_rest what rest;
        (true, body_terms)
    | _ -> error (what ^ " requires a module ... end or sig ... end block")
  in
  split_statements body_terms
  |> List.map (fun stmt ->
         let name, typ_terms =
           match drop_separators stmt with
           | { datum = Token { kind = Ident name; _ }; _ } :: sep :: typ_terms
             when (module_syntax && token_kind Equals sep) || ((not module_syntax) && token_kind Colon sep) ->
               (name, typ_terms)
           | _ ->
               if module_syntax then error ("expected " ^ what ^ " module field of the form name = Type")
               else error ("expected " ^ what ^ " sig field of the form name : Type")
         in
         let (typ : Syntax.t) = parse_type_terms typ_terms in
         match typ.kind with
         | Syntax.Arrow _ -> (name, typ)
         | _ -> error (what ^ " fields must be function types"))

and parse_effect_ops op_terms =
  parse_module_type_fields "effect" op_terms
  |> List.map (fun (name, (typ : Syntax.t)) ->
         match typ.kind with
         | Syntax.Arrow (Explicitness.Explicit, _, input, None, output) ->
             { Syntax.name; input; output }
         | Syntax.Arrow (_, _, _, Some _, _) -> error "effect operation types cannot have latent effects"
         | Syntax.Arrow _ -> error "effect operation types must be explicit function types"
         | _ -> error "effect operation requires a function type")

and parse_trait_fields field_terms =
  parse_module_type_fields "trait" field_terms

and parse_decl_type_params what name_span param_terms =
  match drop_separators param_terms with
  | [] -> []
  | [ ({ datum = Group (Raw_syntax.Paren, items, _); _ } as group) ] ->
      require_adjacent_span name_span group.span (what ^ " parameter list");
      let items = drop_separators items in
      if items = [] then error (what ^ " parameter list cannot be empty")
      else
      split_commas items
      |> List.map (fun item ->
             match drop_separators item with
             | [ { datum = Token { kind = Ident p; _ }; span } ] -> id ~span p
             | _ -> error (what ^ " parameter list expects identifiers"))
  | _ -> error (what ^ " parameters must be written as (A, B)")

and parse_if start_span terms =
  match split_at_token KwDo terms with
  | None -> error "if requires do/else/end syntax"
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
        | Token { kind = KwUnit; _ } -> (Syntax.PatType Atom_ty.TUnit, rest)
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
            match drop_separators items with
            | [] -> (Syntax.PatAtom Atom.Unit, rest)
            | items -> (match split_commas items with
            | [ only ] -> (parse_pat_all only, rest)
            | parts -> (Syntax.PatProd (List.map parse_pat_all parts), rest)) )
        | Token { kind = KwStruct; _ } ->
            let body_terms, rest, _span = collect_until_end term.span rest in
            let fields, partial = parse_pat_struct_type_fields body_terms in
            (Syntax.PatStructType { fields; partial }, rest)
        | _ -> error "unsupported pattern in Phase 7B match")
  and parse_pat_postfix lhs terms =
    match drop_separators terms with
    | dot :: field :: rest when token_kind Dot dot ->
        let field_name =
          match token_text field with
          | Some name -> name
          | None -> error "expected pattern name after '.'"
        in
        let lhs =
          match lhs with
          | Syntax.PatCon (path, name, []) -> Syntax.PatCon (path @ [ name ], field_name, [])
          | Syntax.PatBind name -> Syntax.PatCon ([ name.name ], field_name, [])
          | _ -> error "only constructor patterns can be qualified"
        in
        parse_pat_postfix lhs rest
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
    | { datum = Group (Raw_syntax.Brace, items, _); _ } :: rest ->
        let fields, partial = parse_pat_record_fields items in
        let lhs =
          match lhs with
          | Syntax.PatCon (path, name, []) ->
              Syntax.PatRecord { typ_path = path; typ = name; fields; partial }
          | _ -> error "record pattern fields must follow a type name"
        in
        parse_pat_postfix lhs rest
    | term :: rest when is_expr_start term ->
        let arg, rest = parse_pat_atom (term :: rest) in
        let lhs =
          match lhs with
          | Syntax.PatCon (path, name, args) -> Syntax.PatCon (path, name, args @ [ arg ])
          | _ -> error "only constructor patterns can take arguments"
        in
        parse_pat_postfix lhs rest
    | rest -> (lhs, rest)

  and parse_pat_struct_type_fields items =
    let fields, partial =
      split_statements items
      |> List.fold_left
           (fun (fields, partial) part ->
             match drop_separators part with
             | [ { datum = Token { kind = Ident "_"; _ }; _ } ] -> (fields, true)
             | { datum = Token { kind = Ident name; _ }; _ } :: colon :: pat_terms when token_kind Colon colon ->
                 (fields @ [ (name, parse_pat_all pat_terms) ], partial)
             | _ -> error "expected struct type pattern field")
           ([], false)
    in
    (fields, partial)

  and parse_pat_record_fields items =
    let split_record_fields terms =
      let rec go current acc = function
        | [] -> List.rev (List.rev current :: acc)
        | { datum = Token { kind = Comma; _ } | Token { kind = Semi; _ }; _ } as term :: rest
          when is_separator term || token_kind Comma term ->
            go [] (List.rev current :: acc) rest
        | term :: rest -> go (term :: current) acc rest
      in
      go [] [] terms |> List.filter (fun part -> not (List.for_all is_separator part || part = []))
    in
    let fields =
      split_record_fields (drop_separators items)
      |> List.map (fun part ->
             match drop_separators part with
             | [ { datum = Token { kind = Ident name; _ }; _ } ] ->
                 (name, None)
             | { datum = Token { kind = Ident name; _ }; _ } :: eq :: pat_terms
               when token_kind Equals eq ->
                 (name, Some (parse_pat_all pat_terms))
             | _ -> error "expected record pattern field of the form name or name = pat")
    in
    let partial =
      List.exists
        (fun (name, pat_opt) -> String.equal name "_" && Option.is_none pat_opt)
        fields
    in
    let fields = List.filter (fun (name, pat_opt) -> not (String.equal name "_" && Option.is_none pat_opt)) fields in
    (fields, partial)
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
  | None -> error "match requires do/end syntax"
  | Some (scrut_terms, do_kw, rest) ->
      let scrut = parse_all (fun ts -> parse_expr_prec 0 ts) scrut_terms in
      let body_terms, rest, span = collect_until_end do_kw.span rest in
      let branches =
        split_match_branches body_terms
        |> List.map (fun branch_terms ->
               match split_at_arrow branch_terms with
               | Some ({ datum = Token { kind = KwEffect; _ }; _ } :: effect_terms, _, body_terms) ->
                   let (path, op), arg_terms = dotted_id_from_terms effect_terms in
                   let arg_pat = parse_pat_terms arg_terms in
                   let body = parse_all (fun ts -> parse_expr_prec 0 ts) body_terms in
                   Syntax.EffectBranch { effect_path = path; op; arg_pat; body }
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

and parse_effect_row_terms terms =
  match drop_separators terms with
  | [] -> { Syntax.effects = []; tail = None }
  | _ -> (
      match split_at_token Bar terms with
      | Some (effect_terms, _, tail_terms) ->
          let effects =
            match drop_separators effect_terms with
            | [] -> []
            | _ -> List.map (parse_all (fun ts -> parse_expr_prec 0 ts)) (split_commas effect_terms)
          in
          { Syntax.effects = effects; tail = Some (parse_all (fun ts -> parse_expr_prec 0 ts) tail_terms) }
      | None ->
          { Syntax.effects = List.map (parse_all (fun ts -> parse_expr_prec 0 ts)) (split_commas terms); tail = None })

and parse_can_effect_row terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Brace, items, _); _ } :: rest -> (parse_effect_row_terms items, rest)
  | rest ->
      let eff, rest = parse_expr_prec 40 rest in
      ({ Syntax.effects = [ eff ]; tail = None }, rest)

and attach_effects (lhs : Syntax.t) eff =
  match lhs.kind with
  | Syntax.Arrow (expl, name, dom, None, cod) -> { lhs with kind = Syntax.Arrow (expl, name, dom, Some eff, cod) }
  | Syntax.Arrow _ -> error "duplicate effect annotation"
  | _ -> error "can annotation requires an arrow"

and parse_ref start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg items in
      (stx ~span:(span_between start_span span) (Syntax.RefNew arg), rest)
  | term :: _ when is_expr_start term ->
      let arg, rest = parse_expr_prec 41 terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.RefNew arg), rest)
  | _ -> error "ref requires an argument"

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
  | term :: _ when is_expr_start term ->
      let arg, rest = parse_expr_prec 41 terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.Resume arg), rest)
  | _ -> error "resume requires an argument"

and parse_perform start_span terms =
  let (path, op), rest = dotted_id_from_terms terms in
  let arg, rest = parse_expr_prec 41 rest in
  (stx ~span:(span_between start_span arg.span) (Syntax.Perform { effect_path = path; op; arg }), rest)

and parse_import start_span terms =
  match drop_separators terms with
  | { datum = Token { kind = String path; _ }; span } :: rest ->
      load_syntax_exports path;
      (stx ~span:(span_between start_span span) (Syntax.Import path), rest)
  | _ -> error "import requires a string path"

and parse_module_expr start_span terms =
  match drop_separators terms with
  | do_kw :: _ when token_kind KwDo do_kw ->
      error "module do syntax is not supported; use module ... end"
  | _ ->
      let body_terms, rest, span = collect_until_end start_span terms in
      let bindings = parse_module_bindings body_terms in
      (stx ~span:(span_between start_span span) (Syntax.Module { bindings }), rest)

and parse_sig_expr start_span terms =
  let body_terms, rest, span = collect_until_end start_span terms in
  let bindings =
    split_statements body_terms
    |> List.map (fun stmt ->
           match drop_separators stmt with
           | { datum = Token { kind = Ident name; _ }; span = name_span } :: colon :: typ_terms when token_kind Colon colon ->
               Syntax.LetBinding { name = id ~span:name_span name; value = parse_type_terms typ_terms; public = true }
           | _ -> error "expected signature field name : type")
  in
  (stx ~span:(span_between start_span span) (Syntax.Module { bindings }), rest)

and parse_struct_expr start_span terms =
  match drop_separators terms with
  | do_kw :: _ when token_kind KwDo do_kw ->
      error "struct do syntax is not supported; use struct ... end"
  | _ ->
      let body_terms, rest, span = collect_until_end start_span terms in
      let con_fields, bindings = parse_struct_items body_terms in
      (stx ~span:(span_between start_span span) (Syntax.Struct { con_fields; bindings }), rest)

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
      | Token { kind = KwLet; _ } -> error "let ... in syntax is not supported; use do blocks and bindings"
      | Token { kind = KwFun; _ } -> error "fun syntax is not supported; use fn"
      | Token { kind = KwMacro; _ } -> error "macro ... in syntax is not supported; use macro declarations in do blocks or modules"
      | Token { kind = KwType; _ } -> error "type ... in syntax is not supported; use do blocks and type declarations"
      | Token { kind = KwTrait; _ } -> error "trait ... in syntax is not supported; use do blocks and trait declarations"
      | Token { kind = KwImpl; _ } -> error "impl ... in syntax is not supported; use do blocks and impl declarations"
      | Token { kind = KwDo; _ } -> parse_do term.span rest
      | Token { kind = KwFn; _ } -> parse_fn term.span rest
      | Token { kind = KwIf; _ } -> parse_if term.span rest
      | Token { kind = KwMatch; _ } -> parse_match term.span rest
      | Token { kind = KwRef; _ } -> parse_ref term.span rest
      | Token { kind = KwDeref; _ } -> parse_deref term.span rest
      | Token { kind = KwResume; _ } -> parse_resume term.span rest
      | Token { kind = KwPerform; _ } -> parse_perform term.span rest
      | Token { kind = KwImport; _ } -> parse_import term.span rest
      | Token { kind = KwModule; _ } -> parse_module_expr term.span rest
      | Token { kind = KwSig; _ } -> parse_sig_expr term.span rest
      | Token { kind = KwStruct; _ } -> parse_struct_expr term.span rest
      | Token { kind = Ident name; _ } -> (
          match Operator_env.find_prefix name with
          | Some op ->
              let rhs, rest = parse_expr_prec op.precedence rest in
              let span = span_between term.span rhs.span in
              let f = var ~span:term.span name in
              let expr =
                if Operator_env.is_dynamic_prefix name then stx ~span (Syntax.MacroCall (f, rhs))
                else ap ~span f Explicitness.Explicit rhs
              in
              (expr, rest)
          | None -> (var ~span:term.span name, rest))
      | Token { kind = Operator name; _ } -> (
          match Operator_env.find_prefix name with
          | Some op ->
              let rhs, rest = parse_expr_prec op.precedence rest in
              let span = span_between term.span rhs.span in
              let f = var ~span:term.span name in
              let expr =
                if Operator_env.is_dynamic_prefix name then stx ~span (Syntax.MacroCall (f, rhs))
                else ap ~span f Explicitness.Explicit rhs
              in
              (expr, rest)
          | None -> unsupported ("unsupported prefix operator: " ^ name))
      | Token { kind = Eof; _ } -> error "unexpected EOF in expression"
      | Token { kind; _ } -> (
          match keyword_name kind with
          | Some name -> unsupported ("unsupported Phase 7A keyword: " ^ name)
          | None -> (
              match punct_name kind with
              | Some name -> error ("unexpected punctuation in expression: " ^ name)
              | None -> error "unexpected token in expression"))
      | Group (Raw_syntax.Bracket, items, span) -> (
          match drop_separators rest with
          | arrow :: cod_terms when token_kind ThinArrow arrow ->
              let params = parse_param_group Explicitness.Implicit items in
              let cod : Syntax.t = parse_all (fun ts -> parse_expr_prec 0 ts) cod_terms in
              let result =
                List.fold_right
                  (fun (p : Syntax.param) (acc : Syntax.t) ->
                     let dom = Option.value ~default:(unit_type ()) p.type_ in
                     stx ~span:(span_between span acc.span)
                       (Syntax.Arrow (Explicitness.Implicit, Some p.name, dom, None, acc)))
                  params cod
              in
              (result, [])
          | _ -> (parse_group_expr Raw_syntax.Bracket items span, rest))
      | Group (delimiter, items, span) -> (parse_group_expr delimiter items span, rest))

and parse_expr_prec min_prec terms =
  let lhs, rest = parse_primary terms in
  parse_postfix_infix min_prec lhs rest

and parse_postfix_infix min_prec lhs terms =
  match terms with
  | term :: _ when is_separator term -> (lhs, terms)
  | term :: rest when token_kind ThinArrow term && min_prec <= 1 ->
      let rhs, rest = parse_expr_prec 1 rest in
      let span = span_between lhs.span rhs.span in
      let lhs =
        match lhs.kind with
        | Syntax.Annotated { inner = { kind = Syntax.Var name; _ }; typ } ->
            stx ~span (Syntax.Arrow (Explicitness.Explicit, Some name, typ, None, rhs))
        | _ -> stx ~span (Syntax.Arrow (Explicitness.Explicit, None, lhs, None, rhs))
      in
      parse_postfix_infix min_prec lhs rest
  | term :: rest when token_kind KwCan term && (min_prec <= 0 || match lhs.kind with Syntax.Arrow _ -> true | _ -> false) ->
      let eff, rest = parse_can_effect_row rest in
      parse_postfix_infix min_prec (attach_effects lhs eff) rest
  | term :: rest when token_kind Colon term ->
      let typ, rest = parse_type_entry rest in
      let lhs = stx ~span:(span_between lhs.span typ.span) (Syntax.Annotated { inner = lhs; typ }) in
      parse_postfix_infix min_prec lhs rest
  | at :: { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest when token_kind At at ->
      let arg = parse_group_arg items in
      let lhs = stx ~span:(span_between lhs.span span) (Syntax.MacroCall (lhs, arg)) in
      parse_postfix_infix min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Paren, items, span); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "function call";
      let args =
        match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args items
      in
      let call_span = span_between lhs.span term.span in
      let lhs = List.fold_left (fun f arg -> ap ~span:call_span f Explicitness.Explicit arg) lhs args in
      parse_postfix_infix min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Bracket, items, _); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "implicit argument list";
      let args = parse_args items in
      let call_span = span_between lhs.span term.span in
      let lhs = List.fold_left (fun f arg -> ap ~span:call_span f Explicitness.Implicit arg) lhs args in
      parse_postfix_infix min_prec lhs rest
  | ({ datum = Group (Raw_syntax.Brace, items, _); _ } as term) :: rest ->
      require_adjacent_postfix lhs term "record construction";
      let items = drop_separators items in
      let call_span = span_between lhs.span term.span in
      let lhs =
        if List.exists (token_kind Equals) items then
          stx ~span:call_span (Syntax.RecordConstruct { typ = lhs; fields = parse_record_expr_fields items })
        else
          let arg = parse_all (fun ts -> parse_expr_prec 0 ts) items in
          ap ~span:call_span lhs Explicitness.Implicit arg
      in
      parse_postfix_infix min_prec lhs rest
  | term :: field :: rest when token_kind Dot term -> (
      match field.datum with
      | Token { kind = Int i; _ } ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.Proj (lhs, Int64.to_int i)) in
          parse_postfix_infix min_prec lhs rest
      | _ -> (match token_text field with
      | Some name ->
          let span = span_between lhs.span field.span in
          let lhs = stx ~span (Syntax.FieldAccess (lhs, name)) in
          parse_postfix_infix min_prec lhs rest
      | None -> error "expected field name or projection after '.'"))
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
                else if Operator_env.is_dynamic_infix op.symbol then
                  let arg = stx ~span (Syntax.Prod [ lhs; rhs ]) in
                  stx ~span (Syntax.MacroCall (var ~span:term.span op.symbol, arg))
                else
                  ap ~span
                    (ap ~span (var ~span:term.span op.symbol) Explicitness.Explicit lhs)
                    Explicitness.Explicit rhs
              in
              parse_postfix_infix min_prec lhs rest
          | _ -> (lhs, term :: rest))
        | None ->
            (lhs, term :: rest))
  | [] -> (lhs, [])

and collect_do_body start_span terms =
  let rec go depth acc = function
    | [] -> error "unterminated do block"
    | term :: rest when token_kind KwDo term || token_kind KwSig term -> go (depth + 1) (term :: acc) rest
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

and split_statements terms =
  let is_statement_separator depth term =
    depth = 0 && (is_separator term || token_kind Comma term)
  in
  let rec go depth current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when is_statement_separator depth term -> go depth [] (List.rev current :: acc) rest
    | term :: rest when token_kind KwDo term || token_kind KwSig term ->
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
  go 0 [] [] terms
  |> List.filter (fun stmt -> not (List.for_all is_separator stmt || stmt = []))

and parse_binding_statement stmt =
  match parse_value_decl_statement stmt with
  | Some decl -> Some decl
  | None -> None

and parse_value_decl_statement stmt =
  match stmt with
  | [ { datum = Token { kind = Ident name; _ }; span = name_span }; eq ] when token_kind Equals eq ->
      error ("missing value for binding: " ^ name ^ " at " ^ Format.asprintf "%a" Source_span.pp name_span)
  | { datum = Token { kind = KwRec; _ }; _ } :: rest -> parse_value_decl_after_prefix ~recursive:true rest
  | rest -> parse_value_decl_after_prefix ~recursive:false rest

and parse_value_decl_after_prefix ~recursive stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwFn; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
       :: rest ->
      let value, rest = parse_fn name_span rest in
      ensure_no_rest "function declaration" rest;
      Some { decl_name = id ~span:name_span name; decl_type = None; decl_value = value; decl_recursive = recursive }
  | name_term :: rest when Option.is_some (binding_name_term name_term) -> (
      let name_id = Option.get (binding_name_term name_term) in
      match split_at_token Equals rest with
      | Some (before_eq, _, value_terms) ->
          if drop_separators value_terms = [] then error ("missing value for binding: " ^ name_id.name);
          let decl_type =
            match drop_separators before_eq with
            | [] -> None
            | colon :: typ_terms when token_kind Colon colon -> Some (parse_type_terms typ_terms)
            | _ -> error "binding parameters are not supported; use fn name(params) syntax"
          in
          let decl_value = parse_all (fun ts -> parse_expr_prec 0 ts) value_terms in
          let decl_value = decl_value in
          Some { decl_name = name_id; decl_type; decl_value; decl_recursive = recursive }
      | None -> None)
  | _ -> None

and parse_type_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwType; _ }; _ } :: { datum = Token { kind = Ident name; _ }; span = name_span } :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, { datum = Group (Raw_syntax.Brace, field_terms, _); _ } :: []) ->
           let params =
             drop_separators param_terms
             |> List.concat_map (function
                 | { datum = Token { kind = Ident p; _ }; span } -> [ id ~span p ]
                 | { datum = Group (Paren, items, _); _ } ->
                     split_commas (drop_separators items)
                     |> List.map (fun ts ->
                         match drop_separators ts with
                         | [ { datum = Token { kind = Ident p; _ }; span } ] -> id ~span p
                         | _ -> error "expected type parameter in parens")
                 | _ -> error "expected type parameter")
           in
           let fields =
            split_statements field_terms
            |> List.map (fun field ->
                   match drop_separators field with
                   | { datum = Token { kind = Ident fname; _ }; _ } :: colon :: typ_terms when token_kind Colon colon ->
                       (fname, parse_type_terms typ_terms)
                   | _ -> error "expected record type field")
          in
          Some (Syntax.RecordTypeBinding { name = id ~span:name_span name; params; fields; public })
      | Some (param_terms, _, ctor_terms) ->
           let params =
             drop_separators param_terms
             |> List.concat_map (function
                 | { datum = Token { kind = Ident p; _ }; span } -> [ id ~span p ]
                 | { datum = Group (Paren, items, _); _ } ->
                     split_commas (drop_separators items)
                     |> List.map (fun ts ->
                         match drop_separators ts with
                         | [ { datum = Token { kind = Ident p; _ }; span } ] -> id ~span p
                         | _ -> error "expected type parameter in parens")
                 | _ -> error "expected type parameter")
           in
            let ctors =
             split_by_top_level_bar ctor_terms
             |> List.map (fun part ->
                    match drop_separators part with
                    | { datum = Token { kind = Ident cname; _ }; span } :: payload_terms -> (
                        match drop_separators payload_terms with
                        | { datum = Group (Raw_syntax.Paren, items, _); _ } :: rest when List.exists (token_kind Comma) items ->
                            let rest = drop_separators rest in
                            if rest <> [] then error "unexpected terms after constructor payload";
                            let types = List.map (fun ts -> parse_all parse_type_entry ts) (split_commas (drop_separators items)) in
                            (id ~span cname, types)
                        | _ ->
                            let payload = match drop_separators payload_terms with [] -> [] | terms -> [ parse_type_terms terms ] in
                            (id ~span cname, payload))
                    | _ -> error "expected constructor declaration")
          in
          Some (Syntax.TypeBinding { name = id ~span:name_span name; params; ctors; public })
      | None -> error "type binding requires =")
  | _ -> None

and parse_effect_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwEffect; _ }; _ } :: { datum = Token { kind = Ident name; _ }; span = name_span } :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, op_terms) ->
          let params = parse_decl_type_params "effect" name_span param_terms in
          Some (Syntax.EffectBinding { name = id ~span:name_span name; params; ops = parse_effect_ops op_terms; public })
      | None -> error "effect binding requires =")
  | _ -> None

and parse_trait_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwTrait; _ }; _ } :: { datum = Token { kind = Ident name; _ }; span = name_span } :: rest -> (
      match split_at_token Equals rest with
      | Some (param_terms, _, field_terms) ->
          let params =
            match parse_decl_type_params "trait" name_span param_terms with
            | [ param ] -> [ param ]
            | [] -> error "trait declaration requires exactly one parameter"
            | _ -> error "trait declaration accepts exactly one parameter"
          in
          let fields = parse_trait_fields field_terms in
          Some (Syntax.TraitBinding { name = id ~span:name_span name; params; fields; public })
      | None -> error "trait binding requires =")
  | _ -> None

and parse_impl_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwImpl; _ }; _ } :: rest -> (
      match split_at_token Equals rest with
      | Some (trait_terms, _, module_kw :: module_rest) when token_kind KwModule module_kw ->
          let (trait_path, trait_name), arg_terms = dotted_id_from_terms trait_terms in
          let args =
            match drop_separators arg_terms with
            | [ { datum = Group (Raw_syntax.Paren, items, _); _ } ] -> (
                let items = drop_separators items in
                if items = [] then error "impl argument list cannot be empty";
                match List.map parse_type_terms (split_commas items) with
                | [ arg ] -> [ arg ]
                | _ -> error "impl declaration accepts exactly one trait argument")
            | [] -> error "impl declaration requires a parenthesized trait argument"
            | _ -> error "impl trait argument must be written as (Type)"
          in
          let field_terms, after_struct, _ = collect_until_end module_kw.span module_rest in
          ensure_no_rest "impl binding" after_struct;
          let fields =
            split_statements field_terms
            |> List.map (fun field ->
                   match parse_value_decl_statement field with
                   | Some { decl_name; decl_value; _ } -> (decl_name.name, decl_value)
                   | None -> error "expected impl let field")
          in
          Some (Syntax.ImplBinding { trait_path; trait_name; args; fields; public })
      | Some _ -> error "impl binding requires = module ... end"
      | None -> error "impl binding requires = module ... end")
  | _ -> None

and parse_open_statement stmt =
  match drop_separators stmt with
  | [ { datum = Token { kind = KwOpen; _ }; _ }; { datum = Token { kind = Ident name; _ }; span } ] ->
      Some (id ~span name)
  | _ -> None

and parse_syntax_do_statement stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident s; _ }; _ }
    :: { datum = Token { kind = Ident pfx; _ }; _ }
       :: name_term
          :: eq :: value_terms
    when String.equal s "syntax" && String.equal pfx "prefix" && token_kind Equals eq ->
      let name = match syntax_name name_term with Some name -> name | None -> error "syntax prefix requires an operator or identifier name" in
      Operator_env.register_prefix name 50;
      ignore (parse_all (fun ts -> parse_expr_prec 0 ts) value_terms);
      true
  | { datum = Token { kind = Ident s; _ }; _ }
    :: { datum = Token { kind = Ident ifx; _ }; _ }
       :: name_term
          :: { datum = Token { kind = Int p; _ }; _ }
             :: { datum = Token { kind = Ident assoc_str; _ }; _ }
                :: eq :: value_terms
    when String.equal s "syntax" && String.equal ifx "infix" && token_kind Equals eq ->
      let name = match syntax_name name_term with Some name -> name | None -> error "syntax infix requires an operator or identifier name" in
      let assoc = match String.lowercase_ascii assoc_str with
        | "left" -> Operator_env.Left
        | "right" -> Operator_env.Right
        | _ -> error "syntax infix associativity must be 'left' or 'right'"
      in
      Operator_env.register_infix name (Int64.to_int p) assoc;
      ignore (parse_all (fun ts -> parse_expr_prec 0 ts) value_terms);
      true
  | { datum = Token { kind = Ident s; _ }; _ } :: _ when String.equal s "syntax" ->
      error "unsupported syntax declaration shape in do block"
  | _ -> false

and parse_syntax_decl stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident s; _ }; _ }
    :: { datum = Token { kind = Ident pfx; _ }; _ }
       :: name_term
          :: eq :: value_terms
    when String.equal s "syntax" && String.equal pfx "prefix" && token_kind Equals eq ->
      let name = match syntax_name name_term with Some name -> name | None -> error "syntax prefix requires an operator or identifier name" in
      let prec = 50 in
      Operator_env.register_prefix name prec;
      let value = parse_all (fun ts -> parse_expr_prec 0 ts) value_terms in
      Some { syntax_name = id ~span:name_term.span name; syntax_value = value; syntax_export = Operator_env.Prefix { symbol = name; precedence = prec } }
  | { datum = Token { kind = Ident s; _ }; _ }
    :: { datum = Token { kind = Ident ifx; _ }; _ }
       :: name_term
          :: { datum = Token { kind = Int p; _ }; _ }
             :: { datum = Token { kind = Ident assoc_str; _ }; _ }
                :: eq :: value_terms
    when String.equal s "syntax" && String.equal ifx "infix" && token_kind Equals eq ->
      let name = match syntax_name name_term with Some name -> name | None -> error "syntax infix requires an operator or identifier name" in
      let prec = Int64.to_int p in
      let assoc = match String.lowercase_ascii assoc_str with
        | "left" -> Operator_env.Left
        | "right" -> Operator_env.Right
        | _ -> error "syntax infix associativity must be 'left' or 'right'"
      in
      Operator_env.register_infix name prec assoc;
      let value = parse_all (fun ts -> parse_expr_prec 0 ts) value_terms in
      Some { syntax_name = id ~span:name_term.span name; syntax_value = value; syntax_export = Operator_env.Infix { symbol = name; precedence = prec; associativity = assoc } }
  | { datum = Token { kind = Ident s; _ }; _ } :: _ when String.equal s "syntax" ->
      unsupported "unsupported syntax declaration shape"
  | _ -> None

and scoped_binding_to_expr span stmt body =
  let public, stmt = parse_public_prefix stmt in
  if public then error "pub is not supported inside do blocks";
  match parse_type_binding false stmt with
  | Some (Syntax.TypeBinding { name; params; ctors; _ }) ->
      stx ~span (Syntax.TypeDef { name; params; ctors; body })
  | Some (Syntax.RecordTypeBinding { name; params; fields; _ }) ->
      stx ~span (Syntax.RecordTypeDef { name; params; fields; body })
  | Some _ -> error "unexpected non-type binding"
  | None -> (
      match parse_effect_binding false stmt with
      | Some (Syntax.EffectBinding { name; params; ops; _ }) ->
          stx ~span (Syntax.EffectDef { name; params; ops; body })
      | Some _ -> error "unexpected non-effect binding"
      | None -> (
          match parse_trait_binding false stmt with
          | Some (Syntax.TraitBinding { name; params; fields; _ }) ->
              stx ~span (Syntax.TraitDef { name; params; fields; body })
          | Some _ -> error "unexpected non-trait binding"
          | None -> (
              match parse_impl_binding false stmt with
              | Some (Syntax.ImplBinding { trait_path; trait_name; args; fields; _ }) ->
                  stx ~span (Syntax.ImplDef { trait_path; trait_name; args; fields; body })
              | Some _ -> error "unexpected non-impl binding"
              | None -> error "not a scoped binding")))

and parse_do_body_terms span body_terms =
  with_operator_scope (fun () ->
      let statements = split_statements body_terms in
      match List.rev statements with
      | [] -> error "empty do block"
      | final_stmt :: rev_statements ->
          let statements = List.rev rev_statements in
          List.iter
            (fun stmt ->
              if not (parse_syntax_do_statement stmt) then load_imports_in_terms stmt)
            statements;
          let body = parse_all (fun ts -> parse_expr_prec 0 ts) final_stmt in
          List.fold_right
            (fun stmt acc ->
              match parse_syntax_decl stmt with
              | Some { syntax_name = name; syntax_value = value; _ } ->
                  stx ~span (Syntax.MacroDef { name; value; body = acc })
              | None ->
                match parse_macro_binding false stmt with
                | Some (Syntax.MacroBinding { name; value; public = false }) ->
                    stx ~span (Syntax.MacroDef { name; value; body = acc })
                | Some (Syntax.MacroBinding { public = true; _ }) ->
                    error "pub macro is not supported inside do blocks"
                | Some _ -> error "unexpected non-macro binding"
                | None ->
                match parse_binding_statement stmt with
                | Some { decl_name = name; decl_type = type_; decl_value = value; decl_recursive = recursive } ->
                    stx ~span
                      (Syntax.Let { name; type_; value; body = acc; recursive })
                | None -> (
                    match parse_open_statement stmt with
                    | Some name -> stx ~span (Syntax.Open (name, acc))
                    | None ->
                        scoped_binding_to_expr span stmt acc))
            statements body)

and parse_do start_span terms =
  let body_terms, rest, span = collect_do_body start_span terms in
  (parse_do_body_terms span body_terms, rest)

and parse_public_prefix stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwPub; _ }; _ } :: rest -> (true, rest)
  | rest -> (false, rest)

and parse_syntax_binding public stmt =
  match parse_syntax_decl stmt with
  | Some { syntax_name = name; syntax_value = value; _ } ->
      Some (Syntax.MacroBinding { name; value; public })
  | None -> None

and parse_macro_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwMacro; _ }; _ }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
       :: rest ->
      let value, rest = parse_fn name_span rest in
      ensure_no_rest "macro binding" rest;
      Some (Syntax.MacroBinding { name = id ~span:name_span name; value; public })
  | _ -> None

and parse_named_module_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwModule; _ }; span = module_span }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
       :: do_kw :: body_rest
    when token_kind KwDo do_kw ->
      let body_terms, rest, span = collect_until_end do_kw.span body_rest in
      ensure_no_rest "module declaration" rest;
      let bindings = parse_module_bindings body_terms in
      let value = stx ~span:(span_between module_span span) (Syntax.Module { bindings }) in
      Some (Syntax.LetBinding { name = id ~span:name_span name; value; public })
  | _ -> None

and parse_module_binding stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_syntax_binding public stmt with
  | Some binding -> binding
  | None -> (
          match parse_macro_binding public stmt with
          | Some binding -> binding
          | None -> (
              match parse_type_binding public stmt with
              | Some binding -> binding
              | None -> (
                  match parse_effect_binding public stmt with
                  | Some binding -> binding
                  | None -> (
                      match parse_trait_binding public stmt with
                      | Some binding -> binding
                      | None -> (
                          match parse_impl_binding public stmt with
                          | Some binding -> binding
                          | None -> (
                              match parse_named_module_binding public stmt with
                              | Some binding -> binding
                              | None -> (
                                  match parse_value_decl_statement stmt with
                                  | Some { decl_name = name; decl_type; decl_value; decl_recursive } ->
                                      if decl_recursive then error "recursive module bindings are not supported in Phase 7C modules";
                                      let value =
                                        match decl_type with
                                        | Some typ -> stx ~span:(syntax_span stmt) (Syntax.Annotated { inner = decl_value; typ })
                                        | None -> decl_value
                                      in
                                      Syntax.LetBinding { name; value; public }
                                  | None -> unsupported "unsupported Phase 7C module item")))))))

and parse_module_bindings body_terms =
  with_operator_scope (fun () ->
      let statements = split_statements body_terms in
      List.iter
        (fun stmt ->
          match parse_syntax_decl stmt with
          | Some _ -> ()
          | None -> load_imports_in_terms stmt)
        statements;
      List.map parse_module_binding statements)

and parse_struct_field stmt =
  match drop_separators stmt with
  | [ { datum = Token { kind = Ident name; _ }; _ }; colon ] when token_kind Colon colon ->
      error ("missing type for struct field: " ^ name)
  | { datum = Token { kind = Ident name; _ }; _ } :: colon :: typ_terms when token_kind Colon colon ->
      if List.exists (token_kind Equals) typ_terms then None else Some (name, parse_type_terms typ_terms)
  | _ -> None

and parse_named_struct_binding public stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwStruct; _ }; span = struct_span }
    :: { datum = Token { kind = Ident name; _ }; span = name_span }
       :: do_kw :: body_rest
    when token_kind KwDo do_kw ->
      let body_terms, rest, span = collect_until_end do_kw.span body_rest in
      ensure_no_rest "struct declaration" rest;
      let con_fields, bindings = parse_struct_items body_terms in
      let value = stx ~span:(span_between struct_span span) (Syntax.Struct { con_fields; bindings }) in
      Some (Syntax.LetBinding { name = id ~span:name_span name; value; public })
  | _ -> None

and parse_struct_binding stmt =
  let public, stmt = parse_public_prefix stmt in
  match parse_method_binding public stmt with
  | Some binding -> binding
  | None -> (
  match parse_syntax_binding public stmt with
  | Some binding -> binding
  | None -> (
      match parse_macro_binding public stmt with
      | Some binding -> binding
      | None -> (
          match parse_type_binding public stmt with
          | Some binding -> binding
          | None -> (
              match parse_effect_binding public stmt with
              | Some binding -> binding
              | None -> (
                  match parse_trait_binding public stmt with
                  | Some binding -> binding
                  | None -> (
                      match parse_impl_binding public stmt with
                      | Some binding -> binding
                      | None -> (
                          match parse_named_module_binding public stmt with
                          | Some binding -> binding
                          | None -> (
                              match parse_named_struct_binding public stmt with
                              | Some binding -> binding
                              | None -> (
                                  match parse_value_decl_statement stmt with
                                  | Some { decl_name = name; decl_type; decl_value; decl_recursive } ->
                                      if decl_recursive then error "recursive struct bindings are not supported in Phase 7C structs";
                                      let value =
                                        match decl_type with
                                        | Some typ -> stx ~span:(syntax_span stmt) (Syntax.Annotated { inner = decl_value; typ })
                                        | None -> decl_value
                                      in
                                      Syntax.LetBinding { name; value; public }
                                   | None -> unsupported "unsupported Phase 7C struct item")))))))))

and parse_struct_items body_terms =
  split_statements body_terms
  |> List.fold_left
       (fun (fields, bindings) stmt ->
         match parse_struct_field stmt with
         | Some field -> (fields @ [ field ], bindings)
         | None -> (fields, bindings @ [ parse_struct_binding stmt ]))
       ([], [])

let parse_terms terms = parse_all (fun ts -> parse_expr_prec 0 ts) terms

let parse_expr ?file ?load_syntax source =
  with_syntax_loader load_syntax (fun () ->
  try Raw_syntax.read ?file source |> parse_terms with
  | Raw_syntax.Error msg -> error msg
  )

let parse_type ?file source =
  try Raw_syntax.read ?file source |> parse_all parse_type_entry with
  | Raw_syntax.Error msg -> error msg

let parse_pat ?file source =
  try Raw_syntax.read ?file source |> parse_pat_terms with
  | Raw_syntax.Error msg -> error msg

let parse_public_syntax_exports ?file ?load_syntax source =
  with_syntax_loader load_syntax (fun () ->
      with_operator_scope (fun () ->
          try
            Raw_syntax.read ?file source
            |> split_statements
            |> List.filter_map (fun stmt ->
                   let public, stmt = parse_public_prefix stmt in
                   if public then Option.map (fun decl -> decl.syntax_export) (parse_syntax_decl stmt)
                   else None)
          with Raw_syntax.Error msg -> error msg))

let parse_module ?file ?load_syntax source =
  with_syntax_loader load_syntax (fun () ->
  try
    let bindings = Raw_syntax.read ?file source |> parse_module_bindings in
    stx (Syntax.Module { bindings })
  with Raw_syntax.Error msg -> error msg
  )
