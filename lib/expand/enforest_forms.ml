open Raw_syntax
open Enforest_util

type callbacks = {
  parse_expr_prec : Enforest_util.prec -> Raw_syntax.t list -> Syntax.t * Raw_syntax.t list;
  parse_expr_terms : Raw_syntax.t list -> Syntax.t;
  parse_do_body_terms : Source_span.t -> Raw_syntax.t list -> Syntax.t;
  parse_pat_terms : Raw_syntax.t list -> Syntax.pat;
  parse_items : Raw_syntax.t list -> Syntax.struct_binding list;
  is_expr_start : Raw_syntax.t -> bool;
}

let parse_group_arg callbacks items =
  match drop_separators items with
  | [] -> unit ()
  | _ -> callbacks.parse_expr_terms items

(* [match (scrutinee) { pattern => result, effect E.op x => result }]. *)
let parse_match callbacks start_span terms =
  match drop_separators terms with
  | ({ datum = Group (Raw_syntax.Paren, _, _); _ } as scrut) :: { datum = Group (Raw_syntax.Brace, arms, span); _ } :: rest ->
      let scrut = callbacks.parse_expr_terms [ scrut ] in
      let branches =
        split_match_branches arms
        |> List.map (fun branch_terms ->
               match split_at_fat_arrow branch_terms with
               | Some ({ datum = Token { kind = KwEffect; _ }; _ } :: effect_terms, _, body_terms) ->
                   let op, arg_terms = path_from_terms effect_terms in
                   let arg_pat = callbacks.parse_pat_terms arg_terms in
                   let body = callbacks.parse_expr_terms body_terms in
                   Syntax.EffectBranch { op; arg_pat; body }
               | Some (pat_terms, _, body_terms) ->
                   let pat = callbacks.parse_pat_terms pat_terms in
                   let body = callbacks.parse_expr_terms body_terms in
                   Syntax.ValueBranch (pat, body)
               | None -> error "match arm requires => between pattern and result")
      in
      if branches = [] then error "match requires at least one arm";
      (stx ~span:(span_between start_span span) (Syntax.Match (scrut, branches)), rest)
  | _ -> error "match is written match (scrutinee) { pattern => result, … }"

(* [_] where a row or its tail is written: the rest of the row is inferred. *)
let is_wildcard_term (term : Raw_syntax.t) = match term.datum with Token { kind = Ident "_"; _ } -> true | _ -> false

let parse_effect_row_terms callbacks terms =
  match drop_separators terms with
  | [] -> { Syntax.effects = []; tails = []; inferred = false; polymorphic = false }
  | _ -> (
      match split_at_token Bar terms with
      | Some (effect_terms, _, tail_terms) ->
          let effects =
            match drop_separators effect_terms with
            | [] -> []
            | _ -> List.map callbacks.parse_expr_terms (split_commas effect_terms)
          in
          (match drop_separators tail_terms with
           | [ wild ] when is_wildcard_term wild -> { Syntax.effects = effects; tails = []; inferred = true; polymorphic = false }
           (* Several row variables after the bar: the row is their union. *)
           | _ ->
               { Syntax.effects = effects;
                 tails = List.map callbacks.parse_expr_terms (split_commas tail_terms);
                 inferred = false; polymorphic = false })
      | None -> (
          match drop_separators terms with
          | [ wild ] when is_wildcard_term wild -> { Syntax.effects = []; tails = []; inferred = true; polymorphic = false }
          | _ -> { Syntax.effects = List.map callbacks.parse_expr_terms (split_commas terms); tails = []; inferred = false; polymorphic = false }))

let parse_ref callbacks start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg callbacks items in
      (stx ~span:(span_between start_span span) (Syntax.RefNew arg), rest)
  | term :: _ when callbacks.is_expr_start term ->
      let arg, rest = callbacks.parse_expr_prec Tight terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.RefNew arg), rest)
  | _ -> error "ref requires an argument"

(* [quote(form)] and [quote { items }]: syntax written literally (M10), read
   as quoted syntax by [callbacks]. Each [$x] becomes an id spelled ["$x"]
   where it stands, so the form parses as written, and a reference [x] in the
   hole list, so the macro's own variable is resolved like any other. *)
let parse_quote (callbacks : string list -> callbacks) start_span terms =
  (* The quote's holes: those no rule inside it binds (M9: a hole resolves to
     its nearest binder), each a reference to the macro's variable. *)
  let quoted items =
    let written = ref [] in
    let on_hole name term = if not (List.mem_assoc name !written) then written := (name, term) :: !written in
    let items = Enforest_template.rewrite_holes ~on_hole items in
    let free = Enforest_template.replacement_holes items in
    let holes =
      List.rev !written
      |> List.filter (fun (name, _) -> List.mem name free)
      |> List.map (fun (name, term) -> ("$" ^ name, var_of term name))
    in
    (items, holes, callbacks free)
  in
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let items, holes, callbacks = quoted items in
      let template = parse_group_arg callbacks items in
      (stx ~span:(span_between start_span span) (Syntax.Quote { template; holes }), rest)
  | { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest ->
      let items, holes, callbacks = quoted items in
      (stx ~span:(span_between start_span span) (Syntax.QuoteDecls { items = callbacks.parse_items items; holes }), rest)
  | _ -> error "quote is written quote(expression) or quote { declarations }"

let parse_deref callbacks start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg callbacks items in
      (stx ~span:(span_between start_span span) (Syntax.RefGet arg), rest)
  | _ -> error "deref requires a parenthesized argument"

let parse_resume callbacks start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg callbacks items in
      (stx ~span:(span_between start_span span) (Syntax.Resume arg), rest)
  | term :: _ when callbacks.is_expr_start term ->
      let arg, rest = callbacks.parse_expr_prec Tight terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.Resume arg), rest)
  | _ -> error "resume requires an argument"

let parse_perform callbacks start_span terms =
  let op, rest = path_from_terms terms in
  let arg, rest = callbacks.parse_expr_prec Tight rest in
  (stx ~span:(span_between start_span arg.span) (Syntax.Perform { op; arg }), rest)

(* The unit's roles arrive when expansion reaches the import. *)
let parse_import ~scope start_span terms =
  match drop_separators terms with
  | { datum = Token { kind = String path; _ }; span } :: rest ->
      (stx ~span:(span_between start_span span) (Syntax.Import { path; scope }), rest)
  | _ -> error "import requires a string path"
