# Enforester Improvements

Two complementary phases:

1. **Structured errors & fault-tolerant parsing** — spans, recover, incremental
2. **Spec-oriented structure** — declarative combinators, generic driver

## Phase 1: Error Messages & Fault-Tolerant Parsing

## Current state

`enforest_util.ml:33-34`:

```ocaml
let unsupported msg = raise (Unsupported msg)
let error msg = raise (Error msg)
```

Both are `string -> 'a` — no span, no recovery, parse aborts on first error.

## Step 1: Structured error types

New file: `lib/expand/parse_error.ml`

```ocaml
type kind =
  | Unexpected of { expected : string; got : string }
  | Unconsumed of string       (* first unconsumed token name *)
  | Missing of string          (* required token not found *)
  | Unsupported of string      (* Phase 7X feature *)
  | Unmatched of string        (* unclosed group/block *)

type t = {
  kind : kind;
  span : Source_span.t;
}

type 'a result = Ok of 'a | WithErrors of 'a * t list
```

## Step 2: Error accumulator in parser env

Add to `enforest_util.ml`:

```ocaml
type env = {
  ...
  errors : Parse_error.t list ref;
}
```

Keep existing `error`/`unsupported` functions — they still abort eagerly for code paths that haven't migrated. New code paths push errors and continue.

## Step 3: Recovery strategies

New file: `lib/expand/parse_recover.ml`

```ocaml
val skip_to_statement_boundary : Raw_syntax.t list -> Raw_syntax.t list
(** Skip tokens until a semi/end at depth 0 *)

val skip_to_close : Raw_syntax.token_kind list -> Raw_syntax.t list -> Raw_syntax.t list  
(** Skip until a matching close token (RParen, RBracket, RBrace, KwEnd) *)

val skip_to : (Raw_syntax.t -> bool) -> Raw_syntax.t list -> Raw_syntax.t list
```

## Step 4: Migration helpers

Add convenience functions to `enforest_util.ml`:

```ocaml
val push_error : env -> Source_span.t -> Parse_error.kind -> unit
val push_and_recover : env -> Source_span.t -> Parse_error.kind -> Raw_syntax.t list -> Raw_syntax.t list
```

Pattern: replace `error "unconsumed terms"` with `push_and_recover env span (Unconsumed first) rest`.

## Step 5: Gradual migration

One parser function at a time:

1. `parse_macro_call_binding` — simplest, good test bed
2. `parse_fn_parts` — medium complexity, rest-handling bugs
3. `parse_do_body_terms` — wrapper chains, skip-to-end recovery
4. `parse_expr_prec` — operator precedence, careful recovery
5. Remainder of binding parsers

Each migration:
- Replace `error`/`unsupported` with `push_error` + recovery
- Return `None`/`[]` on error instead of raising
- Update callers to handle partial results

## Status

Every checkbox below described the deleted prototype. Measured against
`src/Fun.Expand` at `dfa519b` (2026-10-02), each claim's state is:

| checkbox (prototype) | measured state in the port |
| --- | --- |
| Phase 1: `Parse_error` module | none — three message-only exception types (`ReaderException`, `ExpandException`, `RoleException`); see the inventory below |
| Phase 1: error accumulator | **ruled out of scope** (2026-10-01, [scope-enforester-improvements](../tickets/scope-enforester-improvements.md)); no program in the suite reports two errors |
| Phase 1: recovery helpers | **ruled out of scope**, same ruling; the only non-advance guard is `Enforest.RequireAdvance` |
| Fix: `parse_fn_parts` arrow body rest handling | `Enforest.EnsureNoRest` refuses a leftover rather than returning one |
| Fix: `unwrap_stx_decl` DeclNil/DeclCons option return | no such function; not applicable |
| Phase 2: `parse_spec.ml` combinator library, generic driver | none — `grep -niE 'spec\|combinator\|pratt' src/Fun.Expand/*.cs` = **0 hits** |
| Phase 3: leaf parsers migrated to specs (6 named) | no specs to migrate to; the six names are prototype parsers (`enforest*.ml`), deleted with it |
| Phase 4/5: migrate binding parsers / Pratt driver | never started; expression reading is role-driven named order groups (`Enforest.cs`, `Enforest.Roles.cs`), a different shape |

## Error-site inventory (2026-10-02)

The error surface, measured — not a plan. Commands and their output:

`grep -rn 'throw new' src/Fun.Expand/*.cs | wc -l` = **173**

`grep -rhoE 'throw new [A-Za-z]+Exception' src/Fun.Expand/*.cs | sort | uniq -c`:

| exception | sites | what it is |
| --- | --- | --- |
| `ExpandException` | 145 | a language error: source the enforester refuses |
| `ReaderException` | 12 | a language error: source that does not read as tokens and groups |
| `RoleException` | 2 | a language error: a role conflict genuine whatever the prelude binds |
| `InvalidOperationException` | 12 | an internal invariant — never a language error |
| `NotImplementedException` | 2 | an unported path, deliberately distinguishable |

`grep -rc 'throw new' src/Fun.Expand/*.cs` (29 files; the six with 0 included):

| file | sites | file | sites |
| --- | --- | --- | --- |
| `Enforest.Roles.cs` | 39 | `Expander.Roles.cs` | 5 |
| `Enforest.cs` | 24 | `Enforest.Macros.cs` | 4 |
| `Enforest.Match.cs` | 17 | `Expander.cs` | 4 |
| `Enforest.Effects.cs` | 15 | `Expander.Match.cs` | 3 |
| `Enforest.Traits.cs` | 14 | `Enforest.Enum.cs` | 2 |
| `Reader.cs` | 12 | `Enforest.Export.cs` | 2 |
| `Expander.Macros.cs` | 9 | `Enforest.Rec.cs` | 2 |
| `Enforest.Patterns.cs` | 6 | `Enforest.Refs.cs` | 2 |
| `Enforest.Structs.cs` | 6 | `Expander.Traits.cs` | 2 |
| `BinderTable.cs` | 1 | `Enforest.Implicits.cs` | 1 |
| `Enforest.Imports.cs` | 1 | `Expander.Imports.cs` | 1 |
| `Expander.Patterns.cs` | 1 | `Expander.Effects.cs` | 0 |
| `Expander.Export.cs` | 0 | `Expander.Rec.cs` | 0 |
| `Expander.Structs.cs` | 0 | `MacroRuntime.cs` | 0 |
| `Terms.cs` | 0 | | |

Span coverage at this base: **0 of 173** sites pass a `SourceSpan` — the
span-carrying work is not in this tree. It lands separately
(`span-on-expansion-errors`): the three language-error types take an optional
span and print ` at <span>`, the shape the elaborator's `Budget.Where()` already
prints. Nothing here re-opens recovery or the accumulator: the ruling stands
that expanding stops at the first error.

### Combinators available

| Combinator | Description |
|-----------|-------------|
| `pure x` | Always succeeds with x |
| `map spec f` | Transform result value |
| `bind spec f` | Sequence dependent on result |
| `seq a b` | Match a then b |
| `seq3/seq4/seq5` | Match 3/4/5 specs in sequence |
| `alt specs` | Try alternatives in order |
| `opt spec` | Optional match, returns None on failure |
| `many spec` | Zero or more |
| `eof` | Expect end of tokens |
| `token pred mk` | Match single token by predicate |
| `punct kind` | Match specific token kind |
| `ident` | Match ident → (Syntax.id, span) |
| `str_ident` | Match ident → (string, span) |
| `group delim spec` | Match Group(delim, ...) with inner spec |
| `paren_group spec` | shortcut for group(Paren, spec) |
| `spanned spec` | Like spec but returns (result, token_span) |
| `custom_spec ~name f` | Ad-hoc spec from run function |
| `drop_sep spec` | Auto-drop separators before matching |
| `recover spec` | Push error on failure, return None |
| `to_option spec` | parse + eof, return Some/None |
| `parse spec` | = drop_sep spec |

### How to migrate a parser

```ocaml
(* Before *)
and parse_foo env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = KwFoo; _ }; _ } :: tokens ->
      ... manual rest tracking ...
  | _ -> None

(* After *)
and parse_foo env stmt =
  let header = Parse_spec.seq (Parse_spec.punct KwFoo) Parse_spec.str_ident in
  match Parse_spec.parse header env stmt with
  | Some (((), (name, span)), rest) -> ... manual body ...
  | None -> None
```

## Known bugs

- **newline after `)` in module statements**: expressions ending with `)`
  followed by newline can confuse the raw lexer. Use `;` separators
  between module statements as a workaround.

## Phase 2: Spec-oriented enforester structure

### Goal

Replace the current hand-rolled recursive-descent pattern with a declarative
spec language driven by a generic token-stream driver. Each parser function
becomes a short spec that describes *what* to parse; the driver handles
separator dropping, depth tracking, rest checking, and now — error recovery.

### Current cost

| Pattern | Occurrences in enforest.ml |
|---------|---------------------------|
| `drop_separators` | 75 |
| `datum ... Token { kind = ... }` | 67 |
| `ensure_no_rest` | 11 |
| `collect_until_end` | 11 |
| Recursive parse functions | 81 |
| Total lines | 1515 |

### Proposed `lib/expand/parse_spec.ml` (~250 lines)

A combinator library:

```ocaml
(* Core combinators *)
val token  : token_kind -> (token -> 'a) -> 'a spec
val ident  : (id * span -> 'a) -> 'a spec
val punct  : token_kind -> unit spec
val group  : delimiter -> 'a spec -> (span -> 'a -> 'b) -> 'b spec
val seq    : 'a spec -> 'b spec -> 'c spec -> ...  (* chain *)
val alt    : 'a spec list -> 'a spec
val many   : 'a spec -> 'a list spec
val maybe  : 'a spec -> 'a option spec
val opt    : 'a spec -> 'a option spec   (* Alt that returns None on failure *)
val eof    : unit spec                   (* ensure consumed *)

(* Precedence-aware expression parsing *)
val expr   : int -> Syntax.t spec        (* Pratt parser driven by spec *)

(* Recovery-aware *)
val recover : 'a spec -> 'a spec  (* on failure: push error, skip, retry *)

(* Run *)
val parse : 'a spec -> env -> Raw_syntax.t list -> 'a * Raw_syntax.t list
```

### Example: macro call binding (19 → 9 lines)

Before:
```ocaml
and parse_macro_call_binding env stmt =
  match drop_separators stmt with
  | { datum = Token { kind = Ident name; _ }; span = name_span }
    :: { datum = Token { kind = At; _ }; _ }
    :: { datum = Group (Raw_syntax.Paren, items, span); _ }
    :: rest ->
      let args = match drop_separators items with
        | [] -> [ unit ~span () ]
        | _ -> parse_args env items in
      ensure_no_rest "macro call binding" rest;
      let f = var ~span:name_span name in
      Some (Syntax.MacroCallBinding { f; args })
  | _ -> None
```

After:
```ocaml
let macro_call_binding_spec =
  opt (
    seq4 ident (punct At) (group Paren (sep_by Comma (expr 0))) eof
    |> map (fun (n, _, args, _) ->
      Syntax.MacroCallBinding { f = var ~span:n.span n.name; args })
  )
```

### How it helps errors

The driver knows at each step what it expected, so error messages are precise:

```
Expected identifier after 'macro', got '42'
  → generated from spec: seq4 (punct KwMacro) ident ...
  → driver saw KwMacro, then Int instead of Ident
```

Recovery becomes a combinator: `recover my_spec` catches failure, pushes the
error with the spec's expected-token info, skips to a statement boundary,
and returns `None`. The caller gets partial results + accumulated errors.

### Migration strategy

1. Build `parse_spec.ml` alongside existing code — does not touch enforest
2. Port one function at a time, starting with leaf parsers:
   - `parse_macro_call_binding` → spec
   - `parse_pattern_syn_binding` → spec
   - `parse_open_statement` → spec
   - `parse_binding_statement` → spec
   - `parse_do_body_terms` → spec (wrapper chain becomes fold)
   - `parse_expr_prec` → Pratt driver driven by spec
3. Remove migrated functions from enforest.ml
4. Delete old boilerplate helpers once unused

### Non-goals (for now)

- Menhir integration — deferred, but structured result types are Menhir-friendly
- Incremental/streaming parsing
