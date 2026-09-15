(* Syntax forms (M9): a form's rules are its parse - which tokens a use
   consumes and what each hole captures - and a replacement, quoted syntax
   parsed where the rule is written. A use is the rule that matched and its
   captures; expansion fills the replacement like a macro application. *)

open Raw_syntax
open Enforest_util

(* A hole's kind is written as its reflection type: [$(x : Id)]. *)
let parse_hole_kind = function
  | ("expr" | "block" | "binder" | "ident" | "decl") as kind ->
      error ("hole kinds are written as types (Expr, Block, Id, Decl, Pattern), not " ^ kind)
  | kind -> (
      match Syntax.hole_kind_of_name kind with
      | Some k -> k
      | None -> error ("unknown syntax template hole kind: " ^ kind))

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

(* An identifier quoted syntax spells [$x]: a hole, after [rewrite_holes]. *)
let hole_ident term =
  match term.datum with
  | Token { kind = Ident name; _ } -> Syntax.hole_name name
  | _ -> None

(* [$x] and [$(x : Kind)] as tokens, rewritten to one identifier spelled [$x] -
   [$] cannot begin a source identifier - so quoted syntax parses as written.
   [on_hole] sees each hole's name. *)
let rewrite_holes ?(on_hole = fun _ _ -> ()) terms =
  let rec go = function
    | { datum = Token { kind = Operator "$"; _ }; _ } :: ({ datum = Token ({ kind = Ident name; _ } as tok); span } as term) :: rest ->
        on_hole name term;
        { datum = Token { tok with kind = Ident ("$" ^ name) }; span } :: go rest
    | term :: rest -> (
        match term.datum with
        | Group (d, items, span) -> { term with datum = Group (d, go items, span) } :: go rest
        | Token _ -> term :: go rest)
    | [] -> []
  in
  go terms

let parse_hole = function
  | { datum = Token { kind = Operator "$"; _ }; _ } :: { datum = Token { kind = Ident name; _ }; span } :: rest ->
      Some (Syntax.PartHole { hole = name; hole_kind = Syntax.HoleExpr; hole_span = span }, rest)
  | { datum = Token { kind = Operator "$"; _ }; _ } :: { datum = Group (Raw_syntax.Paren, items, _); span } :: rest -> (
      match drop_separators items with
      | [ { datum = Token { kind = Ident name; _ }; _ }; colon; { datum = Token { kind = Ident kind; _ }; _ } ]
        when token_kind Colon colon ->
          Some (Syntax.PartHole { hole = name; hole_kind = parse_hole_kind kind; hole_span = span }, rest)
      | _ -> error "expected template hole annotation $(name : Kind)")
  | ({ datum = Token _; span } as term) :: rest when Option.is_some (hole_ident term) ->
      Some (Syntax.PartHole { hole = Option.get (hole_ident term); hole_kind = Syntax.HoleExpr; hole_span = span }, rest)
  | _ -> None

let rec parse_pattern_parts terms =
  let rec go acc = function
    | [] -> List.rev acc
    | terms -> (
        match parse_hole terms with
        | Some (part, rest) -> go (part :: acc) rest
        | None -> (
            match terms with
            | { datum = Group (delimiter, items, span); _ } :: rest ->
                go (Syntax.PartGroup (delimiter, parse_pattern_parts items, span) :: acc) rest
            | term :: rest -> go (Syntax.PartToken term :: acc) rest
            | [] -> List.rev acc))
  in
  go [] (drop_separators terms)

(* A rule's pattern. Its head is a literal: a hole written there - in a rule a
   replacement declares - names the head with an enclosing capture, so it stays
   a token (M7 decision 6). *)
let parse_pattern terms =
  match drop_separators terms with
  | ({ datum = Token _; _ } as head) :: rest when Option.is_some (hole_ident head) ->
      Syntax.PartToken head :: parse_pattern_parts rest
  | terms -> parse_pattern_parts terms

let pattern_holes parts =
  let rec go acc = function
    | [] -> acc
    | Syntax.PartHole { hole; _ } :: rest ->
        if List.mem hole acc then error ("duplicate syntax pattern hole: " ^ hole);
        go (hole :: acc) rest
    | Syntax.PartGroup (_, parts, _) :: rest -> go (go acc parts) rest
    | Syntax.PartToken _ :: rest -> go acc rest
  in
  go [] parts

let is_syntax_keyword = function
  | { datum = Token { kind = Ident "syntax"; _ }; _ } -> true
  | _ -> false

(* [: Decl] between a syntax form's head and its rules. *)
let syntax_kind = function
  | colon :: { datum = Token { kind = Ident "Decl"; _ }; _ } :: rest when token_kind Colon colon ->
      (Syntax.MacroAnnotation.Decl, rest)
  | rest -> (Syntax.MacroAnnotation.Expr, rest)

(* The holes a replacement uses that its own rules do not bind: they must be
   captures of the rule it belongs to, or of an enclosing one. *)
let rec replacement_holes ?(bound = []) terms =
  let rec go acc = function
    | [] -> acc
    | ({ datum = Token _; _ } as term) :: rest when Option.is_some (hole_ident term) ->
        let name = Option.get (hole_ident term) in
        go (if List.mem name bound then acc else name :: acc) rest
    | syntax_kw :: head :: after when is_syntax_keyword syntax_kw -> (
        let acc = match hole_ident head with Some n when not (List.mem n bound) -> n :: acc | _ -> acc in
        match syntax_kind after with
        | _, { datum = Group (Raw_syntax.Brace, body, _); _ } :: rest -> go (nested_rule_holes bound acc body) rest
        | _, rest -> go acc rest)
    | { datum = Group (_, items, _); _ } :: rest -> go (replacement_holes ~bound items @ acc) rest
    | _ :: rest -> go acc rest
  in
  go [] terms

and nested_rule_holes bound acc body =
  split_match_branches body
  |> List.fold_left
       (fun acc rule_terms ->
         match split_at_fat_arrow rule_terms with
         | Some (pattern_terms, _, replacement) ->
             let pattern = parse_pattern pattern_terms in
             let acc =
               match pattern with
               | Syntax.PartToken head :: _ -> (
                   match hole_ident head with Some n when not (List.mem n bound) -> n :: acc | _ -> acc)
               | _ -> acc
             in
             replacement_holes ~bound:(pattern_holes pattern @ bound) replacement @ acc
         | None -> acc)
       acc

(* [parse_replacement holes terms] reads a rule's replacement as quoted syntax,
   with [holes] the captures it may use. *)
let parse_rules ~(available : string list) ~head ~parse_replacement body_terms : Syntax.rule list =
  split_match_branches body_terms
  |> List.map (fun rule_terms ->
         match split_at_fat_arrow rule_terms with
         | Some (pattern_terms, _, replacement) ->
             let pattern = parse_pattern (rewrite_holes pattern_terms) in
             (match pattern with
              | Syntax.PartToken t :: _ when raw_token_spelling t = Some head -> ()
              | _ -> error ("syntax branch pattern must start with declared head: " ^ head));
             (match drop_separators replacement with
              | { datum = Token { kind = Ident "multi"; _ }; _ } :: { datum = Group (Raw_syntax.Brace, _, _); _ } :: _ ->
                  error "multi { … } was removed; declare the syntax form : Decl and write its replacement { … }"
              | _ -> ());
             let holes = pattern_holes pattern in
             let replacement = rewrite_holes replacement in
             List.iter
               (fun name ->
                 if not (List.mem name (holes @ available)) then
                   error ("unbound syntax template hole in replacement: " ^ name))
               (replacement_holes replacement);
             { Syntax.pattern; replacement = parse_replacement (holes @ available) replacement;
               rule_span = syntax_span rule_terms }
         | None -> error "syntax declaration rule requires => between pattern and replacement")

type callbacks = {
  parse_expr : Raw_syntax.t list -> Syntax.t;
  (* An expression at the front of the terms, read at a position, and the
     terms after it. *)
  parse_expr_prefix : prec -> Raw_syntax.t list -> Syntax.t * Raw_syntax.t list;
  (* Where the hole ending a use reads: the form's operand, at its order. A
     hole ending its group or item reads a whole expression ([Top]). *)
  trailing : prec;
  parse_pat_prefix : Raw_syntax.t list -> Syntax.pat * Raw_syntax.t list;
  (* Reading quoted syntax, which is parsed completely where it is written: a
     captured block is read now too (M10). *)
  eager : bool;
}

(* A capture reads as far as its parser does: [read] returns what it read and
   the terms after it. An empty capture matches nothing. *)
let capture read continue input =
  match drop_separators input with
  | [] -> None
  | input ->
      let captured, after = read input in
      continue captured after

(* How far a hole reads is structural (brackets-decide-grouping): the hole
   ending a use reads its form's operand; one ending its group, or followed by
   [,] or [;], reads to that end; any other hole is exactly one term - a token or
   one bracket group. *)
type extent = Trailing | ToSeparator | OneTerm

let extent = function
  | [] -> Trailing
  | Syntax.PartToken t :: _ when token_kind Comma t || is_separator t -> ToSeparator
  | _ -> OneTerm

(* Declarations are captured unread: to the separator, the rest of the use, or
   the items of one brace group. *)
let decl_extent extent input =
  match extent, input with
  | Trailing, _ -> Some (input, [])
  | ToSeparator, _ -> (
      match split_at_pred (fun t -> token_kind Comma t || is_separator t) [] input with
      | Some (before, sep, after) -> Some (before, sep :: after)
      | None -> Some (input, []))
  | OneTerm, { datum = Group (Raw_syntax.Brace, items, _); _ } :: after -> Some (items, after)
  | OneTerm, _ -> None

(* A group's pattern must consume the whole group: a hole ending it must read
   to the group's end. *)
let rec match_group callbacks captures pattern_items input_items =
  Option.map fst (match_parts ~whole:true { callbacks with trailing = Top } captures pattern_items input_items)

and match_parts ?(whole = false) callbacks captures pattern input =
  let continue captures rest input = match_parts ~whole callbacks captures rest input in
  match pattern with
  | [] -> if whole && drop_separators input <> [] then None else Some (captures, input)
  | Syntax.PartToken expected :: rest -> (
      match drop_separators input with
      | actual :: input_rest when same_literal_token expected actual -> continue captures rest input_rest
      | _ -> None)
  | Syntax.PartGroup (delimiter, pattern_items, _) :: rest -> (
      match drop_separators input with
      | { datum = Group (actual, input_items, _); _ } :: input_rest when actual = delimiter -> (
          match match_group callbacks captures pattern_items input_items with
          | Some captures -> continue captures rest input_rest
          | None -> None)
      | _ -> None)
  | Syntax.PartHole { hole; hole_kind; _ } :: rest -> (
      let with_capture c input_rest = continue ((hole, c) :: captures) rest input_rest in
      match hole_kind with
      | Syntax.HoleId -> (
          match drop_separators input with
          | { datum = Token ({ kind = Ident _ | Operator _; _ } as tok); _ } :: input_rest ->
              with_capture (Syntax.CapId tok) input_rest
          | _ -> None)
      | Syntax.HoleBlock -> (
          (* One brace group, captured unread (M9). *)
          match drop_separators input with
          | ({ datum = Group (Raw_syntax.Brace, items, _); _ } as group) :: input_rest ->
              with_capture (if callbacks.eager then Syntax.CapExpr (callbacks.parse_expr [ group ]) else Syntax.CapBlock items) input_rest
          | _ -> None)
      | Syntax.HoleExpr -> (
          match extent rest with
          | Trailing -> capture (fun ts -> let e, after = callbacks.parse_expr_prefix callbacks.trailing ts in (Syntax.CapExpr e, after)) with_capture input
          | ToSeparator -> capture (fun ts -> let e, after = callbacks.parse_expr_prefix Top ts in (Syntax.CapExpr e, after)) with_capture input
          | OneTerm -> capture (fun ts -> (Syntax.CapExpr (callbacks.parse_expr [ List.hd ts ]), List.tl ts)) with_capture input)
      | Syntax.HolePattern -> (
          match extent rest with
          | Trailing | ToSeparator ->
              capture (fun ts -> let p, after = callbacks.parse_pat_prefix ts in (Syntax.CapPattern p, after)) with_capture input
          | OneTerm ->
              capture
                (fun ts ->
                  let p, after = callbacks.parse_pat_prefix [ List.hd ts ] in
                  ensure_no_rest "pattern hole" after;
                  (Syntax.CapPattern p, List.tl ts))
                with_capture input)
      | Syntax.HoleDecl -> (
          (* Declarations, captured unread: they are read where they are spliced. *)
          match decl_extent (extent rest) (drop_separators input) with
          | Some (decls, after) when drop_separators decls <> [] -> with_capture (Syntax.CapDecls [ Syntax.Items decls ]) after
          | _ -> None))

let match_rules callbacks (rules : Syntax.rule list) terms =
  List.find_map
    (fun (rule : Syntax.rule) ->
      Option.map (fun (captures, rest) -> (rule, List.rev captures, rest)) (match_parts callbacks [] rule.pattern terms))
    rules

(* M8: a syntax form is used only where its kind's position is. *)
let check_kind head (kind : Syntax.MacroAnnotation.t) (position : Syntax.MacroAnnotation.t) =
  if kind <> position then
    let name = function Syntax.MacroAnnotation.Expr -> "Expr" | Decl -> "Decl" in
    error (Printf.sprintf "syntax form '%s' has kind %s but was used in %s context" head (name kind) (name position))

let instantiate callbacks ~(form : Syntax.id) ~kind ~position ~from_unit (rules : Syntax.rule list) terms =
  check_kind form.name kind position;
  match match_rules callbacks rules terms with
  | Some (rule, captures, rest) -> ({ Syntax.form; rule; captures; from_unit }, rest)
  | None -> error ("no matching branch for syntax " ^ form.name)
