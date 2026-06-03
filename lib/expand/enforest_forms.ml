open Raw_syntax
open Enforest_util

type callbacks = {
  parse_expr_prec : int -> Raw_syntax.t list -> Syntax.t * Raw_syntax.t list;
  parse_expr_terms : Raw_syntax.t list -> Syntax.t;
  parse_do_body_terms : Source_span.t -> Raw_syntax.t list -> Syntax.t;
  parse_pat_terms : Raw_syntax.t list -> Syntax.pat;
}

let parse_group_arg callbacks items =
  match drop_separators items with
  | [] -> unit ()
  | _ -> callbacks.parse_expr_terms items

let collect_if_branches terms =
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

let parse_if callbacks start_span terms =
  match split_at_token KwDo terms with
  | None -> error "if requires do/else/end syntax"
  | Some (cond_terms, _do_kw, rest) ->
      let cond = callbacks.parse_expr_terms cond_terms in
      let then_terms, else_terms, rest, end_span = collect_if_branches rest in
      let then_ = callbacks.parse_do_body_terms (syntax_span then_terms) then_terms in
      let else_ = callbacks.parse_do_body_terms (syntax_span else_terms) else_terms in
      (stx ~span:(span_between start_span end_span) (Syntax.If { cond; then_; else_ }), rest)

let parse_match callbacks start_span terms =
  match split_at_token KwDo terms with
  | None -> error "match requires do/end syntax"
  | Some (scrut_terms, do_kw, rest) ->
      let scrut = callbacks.parse_expr_terms scrut_terms in
      let body_terms, rest, span = collect_until_end do_kw.span rest in
      let branches =
        split_match_branches body_terms
        |> List.map (fun branch_terms ->
               match split_at_arrow branch_terms with
               | Some ({ datum = Token { kind = KwEffect; _ }; _ } :: effect_terms, _, body_terms) ->
                   let (path, op), arg_terms = dotted_id_from_terms effect_terms in
                   let arg_pat = callbacks.parse_pat_terms arg_terms in
                   let body = callbacks.parse_expr_terms body_terms in
                   Syntax.EffectBranch { effect_path = path; op; arg_pat; body }
               | Some (pat_terms, _, body_terms) ->
                   let pat = callbacks.parse_pat_terms pat_terms in
                   let body = callbacks.parse_expr_terms body_terms in
                   Syntax.ValueBranch (pat, body)
               | None -> error "match branch requires ->")
      in
      if branches = [] then error "match requires at least one branch";
      (stx ~span:(span_between start_span span) (Syntax.Match (scrut, branches)), rest)

let parse_effect_row_terms callbacks terms =
  match drop_separators terms with
  | [] -> { Syntax.effects = []; tail = None }
  | _ -> (
      match split_at_token Bar terms with
      | Some (effect_terms, _, tail_terms) ->
          let effects =
            match drop_separators effect_terms with
            | [] -> []
            | _ -> List.map callbacks.parse_expr_terms (split_commas effect_terms)
          in
          { Syntax.effects = effects; tail = Some (callbacks.parse_expr_terms tail_terms) }
      | None -> { Syntax.effects = List.map callbacks.parse_expr_terms (split_commas terms); tail = None })

let parse_can_effect_row callbacks terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Brace, items, _); _ } :: rest ->
      (parse_effect_row_terms callbacks items, rest)
  | rest ->
      let eff, rest = callbacks.parse_expr_prec 40 rest in
      ({ Syntax.effects = [ eff ]; tail = None }, rest)

let attach_effects (lhs : Syntax.t) eff =
  match lhs.kind with
  | Syntax.Arrow (expl, name, dom, None, cod) -> { lhs with kind = Syntax.Arrow (expl, name, dom, Some eff, cod) }
  | Syntax.Arrow _ -> error "duplicate effect annotation"
  | _ -> error "can annotation requires an arrow"

let parse_ref callbacks start_span terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Paren, items, span); _ } :: rest ->
      let arg = parse_group_arg callbacks items in
      (stx ~span:(span_between start_span span) (Syntax.RefNew arg), rest)
  | term :: _ when is_expr_start term ->
      let arg, rest = callbacks.parse_expr_prec 41 terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.RefNew arg), rest)
  | _ -> error "ref requires an argument"

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
  | term :: _ when is_expr_start term ->
      let arg, rest = callbacks.parse_expr_prec 41 terms in
      (stx ~span:(span_between start_span arg.span) (Syntax.Resume arg), rest)
  | _ -> error "resume requires an argument"

let parse_perform callbacks start_span terms =
  let (path, op), rest = dotted_id_from_terms terms in
  let arg, rest = callbacks.parse_expr_prec 41 rest in
  (stx ~span:(span_between start_span arg.span) (Syntax.Perform { effect_path = path; op; arg }), rest)

let parse_import start_span terms =
  match drop_separators terms with
  | { datum = Token { kind = String path; _ }; span } :: rest ->
      load_syntax_exports path;
      (stx ~span:(span_between start_span span) (Syntax.Import path), rest)
  | _ -> error "import requires a string path"
