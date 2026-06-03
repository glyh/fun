open Raw_syntax
open Enforest_util

let parse_module_type_fields parse_type_terms what terms =
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
             when (module_syntax && token_kind Equals sep)
                  || ((not module_syntax) && token_kind Colon sep) ->
               (name, typ_terms)
           | _ ->
               if module_syntax then error ("expected " ^ what ^ " module field of the form name = Type")
               else error ("expected " ^ what ^ " sig field of the form name : Type")
         in
         let (typ : Syntax.t) = parse_type_terms typ_terms in
         match typ.kind with
         | Syntax.Arrow _ -> (name, typ)
         | _ -> error (what ^ " fields must be function types"))

let parse_effect_ops parse_type_terms op_terms =
  parse_module_type_fields parse_type_terms "effect" op_terms
  |> List.map (fun (name, (typ : Syntax.t)) ->
         match typ.kind with
         | Syntax.Arrow (Explicitness.Explicit, _, input, None, output) ->
             { Syntax.name; input; output }
         | Syntax.Arrow (_, _, _, Some _, _) -> error "effect operation types cannot have latent effects"
         | Syntax.Arrow _ -> error "effect operation types must be explicit function types"
         | _ -> error "effect operation requires a function type")

let parse_trait_fields parse_type_terms field_terms =
  parse_module_type_fields parse_type_terms "trait" field_terms

let parse_decl_type_params what name_span param_terms =
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
