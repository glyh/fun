open Raw_syntax
open Enforest_util

let parse_pat_terms terms =
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
    | term :: rest when is_expr_start (env ()) term ->
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
