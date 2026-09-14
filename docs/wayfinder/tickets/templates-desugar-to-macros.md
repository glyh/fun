---
title: Templates desugar to macros
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
decided: 2026-09-14 (blockers 2 and 3 grilled)
---

# Templates desugar to macros

## Decision (macro model M9, M10)

A template is sugar for a macro. It keeps one job, the parse (patterns,
fixity, what each hole captures); the rest is a macro whose parameters are the
captures and whose body is the replacement as quoted syntax, parsed where it is
written. Hole kinds are the reflection types (`Expr`, `Pattern`, `Decl`,
`Id`), so `binder`/`ident` collapse into `Id` and pattern position gains a
kind.

## Today

Templates instantiate during enforestation, from a `Raw_syntax.t list`
replacement re-parsed at every use site (`Enforest_template`). Correctness no
longer depends on this ticket: template literals resolve at the definition
(template-literals-resolve-at-use-site, closed), through fresh resolved names,
region-aware scope addition for template-introduced ids (`Expand.add_id_scope_if`),
and the defining unit's open as a candidate. What remains is structural: one
instantiation path instead of two, with parse-at-definition for replacements.

## Notes

- The region rule in `Expand.add_id_scope_if` exists only because templates
  instantiate before scopes are added. Once they instantiate during expansion,
  it goes and a binder's scope reaches its whole body.
- Nested template declarations inside a replacement, `multi` declaration
  templates, and inherited captures all ride on token-level rewriting today, and
  must be reconsidered under parse-at-definition.
- Likely sequenced with, or after,
  [template-heads-resolve-by-scope-set](template-heads-resolve-by-scope-set.md).

## Blocked on M7 (found 2026-09-14, attempted implementation)

The desugaring's natural shape exists already: a template declaration becomes
a `MacroSyntaxDecl` whose value is `fn(captures) -> quote(replacement)`, and a
use becomes a `MacroCall` over a `SyntaxOperatorUse`, applied through
`Expand.application` during expansion. That retires the region rule. Three
things stop it from being done cleanly:

1. **Generated syntax must reach later parsing.** Fixity lives only in the
   enforester's string-keyed table (`Binding.add_operator`); the expander never
   registers an operator. Today a template instance runs *during
   enforestation*, so `make_inc; pub result = inc 5` and `pub syntax` emitted
   through `multi` work (tests "7I generated syntax usable later", "7I
   generated pub syntax across imports", "7I generated syntax later-wins").
   Once instances run as macros during expansion, their generated
   `syntax`/`infix` declarations arrive after the following statements were
   parsed. Making the expander hand a syntactic role back to the parser is M7
   (heads as binders, Honu lazy enforestation).
2. **Quote has no declaration position.** `quote(…)` parses an expression and
   its holes are `Expr`/`Pattern`/`Id`. Declaration templates, `$(x: decl)`
   holes (spliced today as raw tokens) and `multi … end` need quoted
   declarations and a `Decl` hole kind.
3. **Nested templates capture outer holes** (`make_adder $base` defining
   `add_base … $x + $base`). As macros this is a quote nested in a quote whose
   inner template mentions an outer hole: quasiquote levels, undecided.
   Macro parameters are also untyped `Expr` syntax, so a `binder`/`ident`
   capture (M10's `Id`) has no typed parameter to arrive through.

Doing only expression and infix templates would leave three instantiation
paths instead of two, and the region rule could not go while declaration
templates still instantiate at parse time.

## Blockers 2 and 3 grilled (2026-09-14)

Blocker 1 is M7's arrangement (grilled, see its ticket). Written in the
[brace syntax](surface-syntax-braces.md):

1. **Declaration quotes.** `quote(…)` quotes one expression; `quote { … }`
   quotes a declaration list with the item grammar of `module { … }`. A hole in
   item position has kind `Decl`. A block is an expression: `quote({ y = 1; y })`.
2. **Nested holes resolve lexically.** `$name` refers to the nearest binder of
   `name` — a template rule's capture, else a macro parameter. Filling an outer
   quote leaves holes bound inside it untouched:
   ```fun
   macro make_adder(base : Expr) : Decl {
     quote { syntax add_base { | add_base $x => $x + $base }; }
   }
   ```
   No quote levels (`$$`); shadowing is resolved by renaming.
3. **Hole kinds are spelled as types** (following M10's one kind set):
   `$(d : Decl)`, `$(name : Id)`, `$(p : Pattern)`, `$(b : Block)`; a bare
   `$v` is `Expr`. `Block` joins the set (M7: a captured `{…}` stays unparsed
   until the output places it). A capture `$(x : T)` is the macro parameter
   `(x : T)` it desugars to; macro parameters take the same annotations.
4. **Templates carry the macro's kind annotation; `multi` is deleted.**
   ```fun
   syntax make_inc : Decl { | make_inc => { syntax inc { | inc $x => $x + 1 }; } }
   ```
   desugars one-to-one to `macro make_inc() : Decl { quote { … } }`.

## Carried from M7 (2026-09-14)

[M7](template-heads-resolve-by-scope-set.md) landed its semantics without
building two pieces whose first consumer is this ticket:

- **The expander-driven loop** (M7 decision 1): once a template is a macro, its
  output can declare syntax, so each form of a definition context must be
  expanded before the next is enforested. Today the enforester reads a whole
  context, minting the scopes roles need itself.
- **Unparsed block captures** (M7 decision 5): `$(b: block)` parses the group
  when it is captured; a macro parameter of kind `Block` needs raw tokens.
- **Name-position holes** already work for declaration templates
  (`syntax $n { | $n $x => … }`, `infix ($op) …`); the macro form needs `Id`
  parameters to carry them.

## Unparsed bodies grilled (2026-09-14)

The expander-driven loop needs bodies to stay unparsed until expansion reaches
them. How that looks to a macro:

1. **A `Block` reflects as a token tree** a macro can read and build — tokens
   and groups, identifier tokens carrying their `Id` with scopes, so the round
   trip stays the identity and hygiene survives:
   ```fun
   macro sql(q : Block) : Expr { match (tokens(q)) { | Tok(IdentTok("SELECT")) :: rest => … } }
   ```
2. **A `Block` hole is a `{…}` in the quoted source**, placeable in any slot
   that takes a brace group, and parsed as that slot expects (statements, module
   items, struct fields):
   ```fun
   syntax namespace { | namespace $(n : Id) $(b : Block) => pub $n = module $b };
   syntax lam { | lam ($(x : Id)) $(b : Block) => fn($x) $b };
   ```
3. **Every `{…}` body inside parsed syntax stays a raw `Block`** until expansion
   reaches it — including bodies nested in an `Expr` argument:
   `twice(run(fn(_) { make_inc inc; inc 5 }))` hands `twice` a `Lam` whose body
   is a `Block`. Outside-in, as Racket and Honu.
4. **`expand_block(b)` expands a block form by form** (Racket's
   `local-expand`) and returns the expanded forms; a macro sees expanded code
   only when it asks. It spends from the running application's budget request.
5. **Expanded forms may be placed back into output; expansion is idempotent.**
   Re-expanding expanded syntax changes nothing — no second rename of resolved
   binders, no duplicate scopes. The invariant gets its own test.

## Implementation run 1 (2026-09-14): landed, then stopped on open questions

**Landed** (`de0a1fd`, suite green):

- Hole kinds are reflection types, written `$(x : Expr | Block | Id | Decl |
  Pattern)`; a bare `$v` is `Expr`. `binder`/`ident` collapse into `Id`, which
  binds or refers by position; a lowercase kind is an error naming the new
  spelling. A `Pattern` capture splices a use-site pattern and its binders.
- `syntax head : Decl { | pat => { items } }`: a syntax form carries the macro
  kind annotation and is used only in its kind's position (an error names the
  mismatch); a `Decl` form's replacement is a brace group of items. `multi` is
  deleted, with an error naming the new form.
- `quote { items }` quotes declarations with the module item grammar
  (`Syntax.QuoteDecls`, reflected `RawQuoteDecls`, typed `Syntax.Decls`); a lone
  `$d` item is a `Decl` hole (`Syntax.HoleBinding`, reflected `DeclHole`).
- The expansion position is the site's, not expander state: an application form
  is an expression, an item-position call is a `MacroCallBinding`. (A `Decl`
  macro now works inside an expression-level `module { … }`.)
- A pattern name is a constructor only when it starts with an uppercase letter;
  a quote hole `$p` in pattern position was a constructor before.

**Not started:** templates as macros, the expander-driven loop, `Block` token
trees, `expand_block`, idempotence, macro parameter kinds (`(x : Id)`), lexical
holes for templates nested in quotes.

**Open questions (need a decision before the loop and templates-as-macros):**

1. *Bodies inside a quote.* Unparsed bodies (decision 3) and typed holes (M10)
   conflict inside quoted syntax. If a quote's bodies stay raw until expansion,
   a hole inside one has no position at the definition, so its kind cannot be
   checked there (`quote({ y = $e; y })`: is `$e` an `Expr`?). If a quote is
   parsed completely at its definition, holes stay typed, but a quoted block
   cannot use syntax that an earlier form of the same block generates by a macro
   call (`{ make_inc inc; inc 5 }` inside a quote or a template replacement).
   Directly written `syntax` declarations inside a quote still work either way.
   Recommendation: parse quotes completely at the definition (M10 as written).
2. *Does a template's macro need the elaborator?* Its body is only a quote. The
   expander can fill it directly (same result, no evaluator), or it can be
   compiled and run like any macro. The second makes every expansion need an
   elaborator: the prelude defines `if`/`&&`/`||` before its own `Syntax` module
   and is expanded with none, and so are `Parse_expand.parse_expr` callers
   (every elaborator test helper, the syntax shape tests). Recommendation: the
   expander fills a template's quote; it is still one application path
   (`Expand.application`).
3. *Where syntax exports come from.* Once a template instance runs during
   expansion, a unit's generated `pub syntax` exists only after expanding the
   unit, but the importer's enforester asks for exports mid-parse
   (`load_syntax_exports`, today an enforester pre-scan that instantiates
   templates). Recommendation: exports come from expanding the unit (the driver
   run `visit_macros` already does), cached; the pre-scan is deleted.
4. *A syntax declaration as data.* `SyntaxBinding` carries only a name; the
   rules live in the enforester's table. A quoted or macro-written `syntax`
   must carry its rules: token-level patterns with holes, and each rule's
   replacement quote, reflected (e.g. `DeclSyntax(Id, Bool, List(Rule))`,
   `Rule = MkRule(List(PatternPart), Expr)`). Needed for `make_adder` as a
   macro and for decision 6's generated syntax.
5. *Idempotence mechanism.* Resolved names are fresh per expander
   (`Expand_ctx.name_counter`), so an expander cannot tell a name another
   expansion minted from a written one. Proposal: one global counter; an id
   whose name was minted is left alone on re-expansion (a binder is not renamed,
   an occurrence not re-resolved), and a labelled open keeps its label.

## Run 1's questions answered (2026-09-14)

1. **A quote is parsed completely at its definition** (M10 holds): holes are
   typed there, and a quoted block cannot use syntax a macro call earlier in the
   same quoted block generates (`quote({ make_inc inc; inc 5 })` is an error at
   the definition; a `syntax` declaration written directly in the quote works).
   User-written bodies stay raw until expansion reaches them.
2. **The expander fills a template's quote directly** — no elaborator. The use
   still goes through `Expand.application`; invariant, tested: filling equals
   evaluating the quote.
3. **Syntax exports come from expanding the unit** (the `visit_macros` driver
   run); the enforester pre-scan is deleted.
4. **`SyntaxBinding` carries its rules as data** — token patterns with holes and
   each rule's quote — reflected both ways.
5. **Resolved names come from one global counter and cannot be written.** A
   minted name is left alone on re-expansion (idempotence) and an open keeps its
   label. A resolved name contains a character no identifier token can contain
   (today `y__0` is a legal identifier), so no written name can ever equal one —
   by construction, not by the freshness of the counter.
