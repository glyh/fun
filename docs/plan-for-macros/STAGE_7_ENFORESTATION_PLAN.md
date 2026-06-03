# Stage 7 Enforestation Plan

## Goal

Stage 7 introduces enforestation as the parsing boundary for the redesigned `fun` surface syntax described in `SYNTAX_SPEC.md`.

This stage should not try to implement every redesigned syntax form at once. The primary goal is to establish the architecture that future syntax extension can build on:

- raw token/tree reading before committed AST parsing;
- syntax classes for expression, type, pattern, declaration/module item, and block/body forms;
- operator tables for prefix, infix, postfix, and delimiter-driven forms;
- a deterministic enforest function that consumes one form at a time;
- a parser pipeline where new syntax enters through enforestation instead of Menhir grammar growth.

## Design References

- `SYNTAX_SPEC.md` is the target surface syntax reference.
- `IMPLEMENTATION_PLAN.md` lines for Stage 7 define the stage-level exit criteria.
- `papers/regular-syntax/honu-enforestation.pdf` is the model for consuming one form from a term stream with operator precedence and syntax extension.
- `SUMMARY.md` describes why enforestation is needed for regular syntax rather than s-expression-only macros.

## Non-Goals For Stage 7

- Do not rewrite elaboration around a new AST.
- Do not implement type-aware, problem-aware, stuck, or type-providing macros.
- Do not add arbitrary user-defined grammar productions yet.
- Do not commit to a full parser redesign in a single patch.

Stage 7 should be implemented as a sequence of small compatibility-preserving slices.

## Pipeline Target

The initial Stage 7 pipeline should be:

```text
source text
  -> raw reader/token trees
  -> enforest one form at a time using built-in syntax/operator tables
  -> Syntax.t
  -> existing hygienic Expand.expand
  -> Lower_surface.lower_expr
  -> Surface.t
  -> existing elaborator/backend
```

The old Menhir compatibility path has been removed. `Parse_expand.parse_expr` and `Parse_expand.parse_module` now route through enforestation directly.

## Phase 7A: Raw Reader And Minimal Expression Enforesting

Status: Implemented.

### Purpose

Create the minimum useful enforestation path without changing existing language behavior.

### Scope

Implement a raw reader that recognizes syntax-spec reader terms:

- integers, strings, chars, identifiers, and operator identifiers;
- delimiters `()`, `[]`, `{}`;
- punctuation `,`, `;`, `.`, `:`, `=`, `|`, `->`, `<-`, `@`;
- newlines as separators;
- line comments `# ...`;
- nested block comments `#| ... |#`;
- datum comments `#_ term`.

Implement an expression enforester that supports only:

- literals;
- identifiers as variables;
- parenthesized expressions;
- tuple expressions `(a, b, c)`;
- curried explicit call syntax `f(a, b, c)` as `f(a)(b)(c)`;
- zero-argument call syntax `f()` as `f(())`;
- curried implicit call syntax `f[A, B]` as `f {A} {B}` internally;
- field access `expr.name`;
- record construction as a postfix brace form, e.g. `Point{x = 1}`;
- record construction after type-function instantiation, e.g. `Pair[I64, Bool]{fst = 1; snd = true}`;
- prefix `not expr`;
- binary arithmetic/comparison operators from the current prelude;
- assignment `<-` lowering to `RefSet` where supported;
- `do ... end` blocks with simple bindings and a final expression.

The first `do`-block slice should accept:

```fun
do
  x = 1
  y = x + 1
  y
end
```

and lower it to nested `Let` nodes ending in the final expression.

### Files

- `lib/syntax/raw_syntax.ml`
- `lib/expand/syntax_class.ml`
- `lib/expand/operator_env.ml`
- `lib/expand/enforest.ml`
- `lib/expand/parse_expand.ml`
- `lib/expand/dune`
- `test/syntax/test_enforest.ml`
- `test/syntax/test_syntax.ml`

### Tests

Add syntax tests for:

- raw grouping for `()`, `[]`, and `{}`;
- `#` and `#| ... |#` comments;
- operator precedence: `1 + 2 * 3`;
- left associativity: `1 - 2 - 3`;
- field access: `m.x.y`;
- curried call: `f(1, 2, 3)`;
- zero-argument call: `f()`;
- implicit call: `id[I64](1)`;
- simple `do` block with two bindings and final expression;
- module bodies where newlines separate declarations;
- unsupported new syntax falls back or fails deterministically.

### Exit Criteria

- Existing syntax tests still pass.
- Existing parser-entrypoint users still parse through `Parse_expand`.
- The supported redesigned expression slice parses through `lib/expand/enforest.ml`.
- `dune exec test/syntax/test_syntax.exe` passes.
- `dune build` passes.

## Phase 7B: Built-In Prefix Forms

Status: Implemented.

### Purpose

Move redesigned built-in expression forms out of Menhir grammar growth and into enforestation entries.

### Scope

Add built-in first-token dispatch for:

- [x] `fn` anonymous functions with `()` explicit params and `[]` implicit params;
- [x] `if cond do ... else ... end`;
- [x] `match scrut do ... end` for the simplest existing value-branch patterns;
- [x] `ref(expr)` and `deref(expr)` syntax;
- [x] `resume expr` and `resume ()`;
- [x] `import "path"`;
- [x] `open M` inside `do` blocks.

`fn` initially supports:

```fun
fn(x : I64) -> x
fn(x : I64) do x end
fn[A : Type](x : A) -> x
fn[A : Type] -> struct value: A; end
fn() -> 1
```

Anonymous functions with only implicit parameters do not require an empty explicit `()` parameter list. `fn()` lowers to a single explicit `Unit` parameter and `f()` lowers to application to unit. References use `deref(r)` for reads; postfix `.deref` is ordinary field access and is not ref syntax.

### Files

- `lib/expand/enforest.ml`
- `lib/expand/operator_env.ml`
- `test/syntax/test_enforest.ml`
- targeted semantic/backend tests only if the new forms should elaborate immediately.

### Tests

- [x] `fn` arrow body and block body;
- [x] implicit params must precede explicit params;
- [x] `if ... do ... else ... end` lowers to `If`;
- [x] simple `match ... do ... end` value branches lower to `Match`;
- [x] `deref(ref(1))` lowers to `RefGet (RefNew ...)` shape;
- [x] `resume ()` lowers to `Resume Unit`;
- [x] `import "x"` shape remains `Import "x"`;
- [x] `open M` works as a statement inside `do` blocks.

### Exit Criteria

- [x] No new final syntax is added to Menhir for these forms.
- [x] Existing old syntax remains compatible.
- [x] The redesigned examples parse to expected `Surface.t` shapes.

## Phase 7C: Declarations And Modules

Status: Implemented for the initial declaration/module parsing slice. Full Honu-style two-pass binding discovery was considered and explicitly deferred; Stage 7 syntax-extension declarations are sequential and affect only later forms.

### Purpose

Introduce declaration/module-item syntax classes so top-level and block parsing can discover bindings before parsing all nested bodies.

### Scope

Support redesigned declaration forms:

- [x] `x = expr`;
- [x] `x : Type = expr`;
- [x] `rec fn f(params) ...` in expression `do` blocks;
- [x] `fn f(params) ...` sugar;
- [x] `fn f(params) -> expr` named arrow-body sugar without a trailing `end`;
- [x] `fn f(params) do ... end` named block-body sugar;
- [x] `fn f[implicit-params] do ... end` named implicit-only block-body sugar;
- [x] `pub method name(params) -> expr` and `pub method name(params) do ... end` inside structs lower to method bindings with `self`/`Self` in scope;
- [x] `pub` modifiers at top-level/module/struct positions;
- [x] `macro name = expr` as the current macro-binding syntax in redesigned modules;
- [x] `module name? do ... end`;
- [x] `struct do ... end` with field declarations and bindings.

The original architectural note for this phase was two-pass scope handling:

- pass 1 detects binding names and registers lexical scopes;
- pass 2 enforests nested bodies using the completed scope for that block.

This mirrors the Honu parse1/parse2 split and prevents order-sensitive macro behavior inside blocks.

Current Stage 7 direction is intentionally simpler: syntax-extension declarations and imports are parsed sequentially, affect only later forms in the enclosing block/module, and use deterministic later-wins shadowing for duplicate operators. A full two-pass declaration pass remains unnecessary unless future syntax extensions need whole-block scope.

The implemented 7C slice parses declarations into `Syntax.t` and still leaves authoritative binding-scope introduction to `Expand.expand`, matching the Stage 7 hygiene boundary. A full pre-enforestation parse1/parse2-style binding discovery pass remains a follow-up before expanding beyond the initial Stage 7E syntax-extension slice.

### Files

- `lib/expand/syntax_class.ml`
- `lib/expand/enforest.ml`
- `lib/expand/expand.ml` if scope introduction needs small adjustments;
- `test/syntax/test_enforest.ml`;
- `test/backend/test_core.ml` for simple evaluation examples.

### Tests

- [x] top-level `pub x = 1` in `parse_module`;
- [x] `do x = 1; x end`;
- [x] `rec fn fact(n : I64) ...` after the expression subset can express the body;
- [x] struct methods using `method`, including zero-extra-argument methods such as `pub method get() do self.value end`;
- [x] module member visibility matches existing `Module`/`Struct` lowering;
- [x] macro declarations are not exposed as runtime fields.

### Exit Criteria

- [x] `parse_module` can parse a small redesigned module body through enforestation.
- [x] Binding scopes are still introduced by `Expand.expand`; enforestation should not replace hygiene.
- [x] Module files parse through enforestation.

## Phase 7D: Type And Pattern Syntax Classes

Status: Implemented.

### Purpose

Separate expression parsing from type and pattern parsing so macros can eventually be problem-aware.

### Scope

Add syntax classes for:

- [x] type expressions: arrows, implicit Pi `[A : Type] -> ...`, applied nominal types `Option(I64)`, qualified types;
- [x] patterns: wildcard, binders, literals, constructors, qualified constructors, tuples, records, qualified records, or-patterns;
- [x] match clauses.

The type parser covers the syntax-spec examples that have direct current `Surface.t` equivalents.

`Enforest` now exports three per-class entrypoints: `parse_expr`, `parse_type`, and `parse_pat`.
Type parsing handles `->` as a right-associative arrow with per-class precedence in `Enforest`
(rather than reusing the expression operator table). Record-pattern brace groups are recognized
in `parse_pat_postfix`, dotted pattern postfixes support qualified constructor and record heads such as `C.Red`, `N.M.X(n)`, and `Alias.Point {x}`, and `parse_type_terms` is used by parameter annotations in `parse_fn`.

### Tests

- [x] `fn(x : Option(I64)) -> x` lowers to type application `Ap(Option, Explicit, I64)`;
- [x] `fn(f : I64 -> Bool) -> f(1)` parses arrow type in param annotation;
- [x] `fn(f : I64 * Bool -> Bool) -> f(1, true)` parses product type `ProdTy`;
- [x] `match x do Some(y) -> y | None -> 0 end` parses constructor payload in match;
- [x] `match x do M.Some(y) -> y | M.None -> 0 end` parses qualified constructor patterns;
- [x] `match p do Point{x; y} -> x end` parses record pattern shorthand;
- [x] `match p do M.Point{x; y} -> x end` parses qualified record patterns;
- [x] `match p do Point{x = n; _} -> n end` parses record pattern renamed field with partial;
- [x] `fn(m : sig x : I64 end) -> m.x` parses signature sugar in parameter annotations.

### Exit Criteria

- [x] Enforestation can request expression/type/pattern parsing explicitly.
- [x] The code has a single place that defines operator precedence per syntax class.

## Phase 7E: User-Visible Syntax Extension

Status: Implemented for the first constrained prefix/infix slice. General Honu-style syntax extension remains future work.

### Purpose

Validate that enforestation is not merely a hard-coded parser replacement.

### Scope

Implement the first constrained syntax-extension bindings:

- [x] one macro-defined prefix form;
- [x] one macro-defined infix operator with precedence and associativity;
- [x] imported syntax extensions use Stage 6 phase-aware macro visits.

This phase is deliberately small. The current API is not the final macro syntax:

```fun
operator prefix name(stx) -> ...
operator infix op precedence left(stx) -> ...
operator infix op precedence right(stx) -> ...
```

The declaration registers a prefix or infix operator during enforestation and expands uses as ordinary macro calls. Prefix and infix syntax pass a dedicated structured syntax-operator-use object to the macro, carrying the operator and parsed operands.

Current implementation notes:

- Dynamic operators live in a per-parse `Operator_env.t` threaded through enforestation and copied for nested `do` blocks/modules so syntax-extension imports do not leak across independent parses.
- `pub operator ...` exports operator declarations through `Core_loader.load_syntax_exports` without exposing them as runtime fields.
- The RHS macro value is parsed and later expanded through the existing `MacroBinding` path; syntax-extension declarations do not introduce arbitrary grammar productions yet.

### Tests

- [x] local prefix syntax extension works;
- [x] local infix operator groups by declared precedence;
- [x] imported public syntax extension works at compile time;
- [x] runtime imports do not expose compile-time syntax-extension bindings as values;
- [x] syntax-extension shadowing is lexical and deterministic.

### Exit Criteria

- [x] At least one prefix form and one infix operator are defined outside hard-coded parser branches.
- [x] The implementation composes with the existing phase-aware loader.
- [x] Ambiguous or unsupported syntax-extension cases produce deterministic errors for the implemented slice.

## Phase 7F: Practical Macro Authoring

Status: Planned. The preceding Stage 7 slices validate the enforestation and syntax-extension plumbing, but the supported macros are still too low-level to be satisfying user-facing macros.

### Purpose

Make Stage 7 syntax extension useful for real macro authors, not just parser plumbing tests. The current `stx_make_*` constructor API is intentionally small but makes every example look like a dummy macro.

This phase should start with all-hygienic syntax-pattern rewrites rather than general quasiquote/unquote. Macro authors should be able to write syntax definitions in the same branch-oriented style as ordinary pattern matching, where pattern holes preserve use-site syntax and replacement identifiers are introduced hygienically.

### Scope

Add a macro authoring layer with:

- [ ] `syntax <head-token> do | <form-template> -> <replacement-template> end` declarations for named/head-position syntax forms, replacing the Stage 7E `operator prefix` spelling;
- [ ] hygienic form-template variables spelled `$name` for simple holes and `$(name: kind)` for explicitly typed holes, capturing use-site syntax for reuse in the replacement;
- [ ] replacement templates that look like ordinary target syntax, without requiring explicit quote/unquote in the common case;
- [ ] a minimal syntax destructuring or inspection API beyond `stx_kind`, enough to consume structured operator-use values when dropping down to computed macro code is necessary;
- [ ] at least one nontrivial syntax-extension macro whose input shape is created by enforestation rather than ordinary function-call syntax.

Named syntax definitions should mirror existing `match` branch syntax:

```fun
syntax unless do
| unless $cond $branch ->
    if $cond do
      ()
    else
      $branch
    end
end

syntax when do
| when $cond $branch else $fallback ->
    if $cond do
      $branch
    else
      $fallback
    end
end
```

Prefix operators are removed in this phase. Prefix/head-position extensions use `syntax <head-token> ...` instead. Infix operators remain a separate `operator` form because the enforester needs precedence and associativity before parsing the operator use. Infix operator declarations do not use `| pattern ->` branch matching because the operator is already known; they take direct operand parameters instead:

```fun
operator infix |> 3 left($lhs, $rhs) -> pipe_apply($lhs, $rhs)
```

Hole spelling for Stage 7F is `$name` for simple holes and `$(name: kind)` when the hole kind must be explicit. General quasiquote/unquote can remain a later usability layer if syntax-pattern replacement templates cover the common cases.

Hygiene rule for this phase:

- pattern variables such as `$cond` and `$branch` preserve use-site syntax and scopes;
- macros expand with their definition lexical environment for introduced identifiers and compile-time dependencies;
- replacement identifiers written without `$` are definition-site identifiers with a fresh expansion scope per macro invocation;
- binders captured from pattern variables bind where they are placed in the replacement;
- literal introduced binders and literal introduced references in the same replacement can bind each other;
- introduced binders from separate macro invocations do not accidentally share local binding identity;
- introduced binders are hygienic unless a future explicit escape hatch says otherwise.

Stage 7F semantics:

- `syntax` declarations are expression-form macros in Stage 7; type, pattern, declaration, and module-item macro contexts are deferred to Stage 8 problem-aware macros;
- `syntax` declarations are allowed anywhere existing compile-time macro/operator declarations are allowed: modules, structs, and local `do` blocks;
- `pub syntax` exports compile-time syntax only from importable module/module-file positions and does not create a runtime field;
- `pub syntax` inside an ordinary runtime `struct ... end` expression is rejected in Stage 7 because it has no meaningful runtime-field interpretation;
- syntax and value declarations share the same source namespace; a later declaration, whether syntax or value, always wins over earlier declarations of the same name;
- a visible syntax binding owns its head position; if no branch matches, expansion reports a syntax error rather than falling back to a same-named value binding;
- a later value declaration with the same name shadows the syntax binding and restores ordinary value lookup;
- public value and syntax exports also use the same source-namespace rule: if both are exported with the same name, later-wins determines the active imported binding and the shadowed binding is not brought into scope by `open`;
- syntax macro expansion may recursively produce another syntax macro use; robust recursion/cycle UX is deferred to Stage 13;
- syntax patterns match raw/enforestation terms owned by the syntax head, not already-lowered `Surface.t`;
- `syntax <head-token>` heads are identifiers only in Stage 7F; keyword overriding and qualified heads are deferred;
- each branch pattern must begin with the declared syntax head token; a branch whose first literal does not match the declared head is a declaration-time error;
- infix operator tokens are handled only by the separate `operator infix` form;
- literal tokens in syntax patterns may be identifiers, keywords, operators, or delimiters; they match by raw token spelling at the same nesting depth and are not hygienically resolved identifiers;
- branches are tried in source order and the first matching branch wins;
- duplicate or overlapping patterns within the same `syntax` declaration are not an ambiguity error because first match wins;
- if a branch match fails, matching continues with the next branch;
- if no branch matches, report a deterministic syntax error naming the syntax head and pointing at the use site plus the declaration span;
- ambiguity across separately visible syntax forms is an error only when the enforester cannot resolve which syntax binding owns the use.
- replacement right-hand sides are parsed in expression-template mode in Stage 7F; broader template contexts are deferred to Stage 8.
- `$name` holes may be used more than once in a replacement; repeated hole names in patterns remain deferred;
- binder holes are not restricted in replacement position in Stage 7; questionable duplicate-binder output can become a warning in a future diagnostics pass;
- reusing an expression hole duplicates syntax rather than sharing evaluation, so any effects in the captured expression happen once per inserted copy;
- `$name` holes used in replacements must have been captured by the branch pattern; otherwise the syntax declaration fails with an unbound template-hole error;
- captured holes that are not used in a replacement are allowed, like unused variables in ordinary pattern matching; warnings are deferred;
- `$name` hole kinds are inferred as `binder` in binder positions, `ident` in identifier-only positions, and `expr` otherwise; ambiguous holes require explicit `$(name: kind)` annotations.

Initial hole kinds:

- expression holes for one expression;
- binder holes for identifiers in binding positions;
- identifier holes for non-binding identifier syntax.

Hole kinds may be inferred from unambiguous pattern positions, but ambiguous holes must use explicit `$(name: kind)` syntax. Initial explicit kinds are `expr`, `binder`, and `ident`.

`do ... end` is not a separate body hole kind in Stage 7F. It remains an expression form equivalent to sequencing/nesting expressions, so an expression hole can capture a whole `do ... end` expression. The pattern form `do $expr end` is allowed and captures the block-body expression represented by the `do ... end` contents. Module/struct declaration holes and declaration-template replacement contexts are split into Phase 7H.

`$` is always parsed as a separate token, not fused with the following identifier. This simplifies tokenization and is useful for macros that generate macros (Phase 7I).

Hole consumption rules in Stage 7F:

- adjacent expression holes are illegal unless separated by literal tokens or delimiters;
- a hole consumes raw terms until the next literal token in the pattern at the same nesting depth;
- expression holes consume the shortest token slice that parses as one expression and still lets the rest of the branch pattern match;
- if more than one successful capture remains genuinely possible, matching fails with an ambiguous-hole error and the macro author must add delimiters or literal separators;
- if the next token after a hole start is `(`, `[`, `{`, or `do`, the hole consumes the balanced content up to the matching `)`, `]`, `}`, or `end`, respecting nesting;
- binder holes cannot appear in expression locations in either pattern or replacement templates;
- a binder hole can only appear in binding positions; an ident hole cannot appear in binder positions.

Deferred from Stage 7F:

- repeated pattern holes and ellipses;
- type, pattern, declaration, and module-item syntax macro contexts;
- explicit non-hygienic escape hatches.

### Tests

- [ ] named `syntax` declarations parse with one or more `| pattern -> replacement` branches;
- [ ] branch patterns must start with the declared syntax head and mismatches are declaration-time errors;
- [ ] `$name` and `$(name: kind)` holes parse, capture use-site syntax, and replacement holes splice it unchanged;
- [ ] captured holes can be used more than once in replacements;
- [ ] binder holes can be reused in replacements without a Stage 7 hard error;
- [ ] reusing expression holes duplicates evaluation after expansion;
- [ ] replacement holes that were not captured by the branch pattern are rejected at declaration time;
- [ ] captured holes that are unused in a replacement are accepted;
- [ ] ambiguous holes require explicit `$(name: kind)` annotations;
- [ ] expression, binder, and identifier holes each have focused parser and rewrite tests;
- [ ] expression holes can capture `do ... end` expressions without a separate body hole kind;
- [ ] adjacent expression holes are rejected with a deterministic error;
- [ ] expression holes use shortest successful expression capture and report genuinely ambiguous captures;
- [ ] `(`, `[`, `{`, and `do` after a hole start consume balanced content up to the matching closing delimiter;
- [ ] binder holes are rejected in expression positions in both pattern and replacement templates;
- [ ] `$` is tokenized separately from the following identifier;
- [ ] literal pattern tokens can be identifiers, keywords, operators, and delimiters;
- [ ] later declarations, whether syntax or value, shadow earlier declarations of the same name;
- [ ] a visible syntax head with no matching branch errors instead of falling back to a same-named value;
- [ ] same-name public value/syntax exports obey later-wins import behavior;
- [ ] `pub syntax` exports compile-time syntax without creating runtime fields;
- [ ] `pub syntax` in an ordinary runtime `struct ... end` expression is rejected deterministically;
- [ ] failed branch matches continue to later branches, and first successful branch wins;
- [ ] no-match errors name the syntax head and include use-site/declaration spans;
- [ ] replacement templates can construct literals, variables, applications, lambdas, and blocks without raw `stx_make_*` calls;
- [ ] replacement templates preserve hygiene for introduced binders and do not capture user bindings;
- [ ] introduced binders are fresh per macro invocation while introduced references in the same replacement resolve to them;
- [ ] `operator prefix` declarations are rejected or migrated to `syntax <head-token>`;
- [ ] an infix `operator` declaration uses direct operand parameters, not `| pattern ->` branches;
- [ ] a named `syntax` macro and an infix `operator` macro each rewrite a real expression using syntax-pattern templates;
- [ ] a macro-generated expression elaborates and evaluates without using raw `stx_make_*` constructors in the macro body.

### Exit Criteria

- [ ] At least one checked-in macro example would be meaningfully worse without enforestation.
- [ ] Macro authoring examples are readable enough to serve as documentation.
- [ ] Existing Stage 7 enforestation, hygiene, loader, and macro tests still pass.

## Phase 7H: Module And Struct Declaration Templates

Status: Planned. Phase 7F handles expression-template syntax macros. This phase adds declaration-template support for module and struct bodies without taking on full Stage 8 problem-aware macro contexts.

### Purpose

Allow syntax macros to match and generate a small, well-defined subset of module/struct declaration terms. Module and struct bodies contain item-like terms that are not ordinary expressions, so they need a separate template context from expression templates.

Stage 7H does not add user-facing same-head expression/declaration overloading. The implementation may store syntax bindings by syntax class internally, but if the same head is declared as both an expression macro and a declaration macro in one visible namespace, the ordinary later-wins rule still decides which declaration is active. Separate expression/declaration bindings for the same head are deferred to Stage 8 problem-aware macro contexts.

### Scope

Add declaration-template support with:

- [ ] `decl` holes for one module/struct declaration term;
- [ ] declaration-template replacement parsing for module/struct contexts;
- [ ] support for public and private value bindings;
- [ ] support for typed value bindings;
- [ ] `decl` holes capture `pub` visibility modifiers and preserve them in replacements;
- [ ] declaration-template macros can expand to multiple declarations in a module/struct body.

Initial `decl` coverage:

```fun
x = expr
x : Type = expr
pub x = expr
pub x : Type = expr
```

`pub` is captured and preserved. For example, if a `$(d: decl)` captures `pub x = 1`, placing `$d` in a replacement decl body produces `pub x = 1`. A declaration-template macro's branch may produce multiple declarations; the expander flattens them into the enclosing module/struct body.

Multi-declaration replacement templates use a declaration-template-only wrapper, `multi ... end`, rather than ordinary expression `do ... end`. This avoids reusing expression-block syntax for a value that should expand to several sibling declarations:

```fun
syntax make_pair do
| make_pair $x $y ->
    multi
      pub first = $x
      pub second = $y
    end
end
```

Bare declaration replacements remain valid for single-declaration output. `multi ... end` is not an expression form and is rejected in expression-template contexts.

Declarations produced by `multi ... end` are spliced as sibling declarations at the macro use site in written order. Later declarations in the same `multi ... end` output can see earlier generated declarations according to the ordinary sequential declaration rules, including generated syntax, macro, and operator declarations once Phase 7I is implemented. No generated declaration is retroactively visible before the macro use site.

Deferred from Stage 7H:

- struct field declarations;
- type/effect/trait/impl declarations;
- macro/syntax/operator declarations generated by macros;
- repeated declaration holes and ellipses.

### Tests

- [ ] `$(name: decl)` captures a module value binding with its `pub` modifier;
- [ ] `$(name: decl)` captures a struct value binding with its `pub` modifier;
- [ ] typed value declarations and `pub` modifiers are preserved through declaration templates;
- [ ] declaration-template replacements can generate public and private value bindings;
- [ ] declaration-template macros can expand to multiple declarations;
- [ ] multi-declaration replacements use `multi ... end` and are rejected in expression-template contexts;
- [ ] `multi ... end` declarations are spliced in written order at the use site;
- [ ] struct field declarations are rejected or explicitly deferred in declaration-template macros.

### Exit Criteria

- [ ] Module and struct declaration-template macros work for value and typed value bindings.
- [ ] Declaration-template contexts remain separate from expression-template contexts.
- [ ] Existing Stage 7 enforestation, hygiene, loader, and macro tests still pass.

## Phase 7G: Computed Hygienic Macro API

Status: Planned. Phase 7F gives the common case a readable, all-hygienic pattern/template surface. This phase adds lower-level computed macro tools without making quote/unquote or user-visible symbol generation the foundation.

### Purpose

Support real computed macros in addition to pattern-matching-style syntax rewrites. Macro authors should be able to construct, inspect, and transform syntax objects directly without relying only on fixed rewrite templates or the current tiny `stx_make_*` constructor set.

Computed macros in this phase should still be hygienic by default. Freshness should come from scoped syntax objects and explicit identifier provenance, not from user-visible `gensym`/`symgen` APIs.

This phase should not replace Phase 7F. The intended layering is:

- 7F: readable all-hygienic syntax-pattern rewrites for common cases;
- 7G: hygienic syntax-object primitives for computed macros and advanced AST manipulation.

### Scope

Add a computed macro authoring layer with:

- [ ] a stable syntax-object inspection API for downstream macro authors;
- [ ] hygienic AST builder/deconstructor primitives that cover the supported Stage 7 surface forms;
- [ ] explicit identifier-provenance APIs for introduced identifiers, preserved input identifiers, and caller-supplied binders;
- [ ] syntax traversal helpers for common transforms, where a raw constructor/deconstructor API would be too painful;
- [ ] computed macro examples that do not use quote/unquote or user-visible symbol generation;
- [ ] documentation that explains when to use 7F pattern/template syntax versus 7G computed macro APIs.

Stage 7G computed macros are still expression macros. Computed type, pattern, declaration, and module-item macros are deferred to Stage 8 problem-aware macro contexts.

Candidate surface examples:

```fun
macro inc(stx) ->
  Syntax.infix("+", stx, Syntax.i64(1))

macro log_call(stx) ->
  do
    name = Syntax.head_name(stx)
    Syntax.do([
      Syntax.call(Syntax.intro_id("print"), Syntax.string("calling " + name)),
      stx,
    ])
  end
```

The public API should use `Syntax.*` names, implemented as a module in the standard library with no special compiler treatment unless strictly necessary. Existing `stx_*` primitives are temporary compatibility/internal names and should be removed or hidden as part of this phase. Quasiquote/unquote can remain future sugar over this API if the builder API is still too verbose. They are not Stage 7G requirements.

### Syntax Object Primitive Families

The primitive API should be organized enough that downstream users can write macros without reverse-engineering internal constructors:

- [ ] classification: `stx_kind`, operator-use kind checks, atom/identifier checks;
- [ ] identifiers: get name, compare hygienic identity, construct introduced identifiers, preserve input identifiers, and place caller-supplied identifiers in binder positions;
- [ ] literals: construct and inspect integer, bool, char, string, and unit syntax;
- [ ] application/lambda/let: construct and deconstruct common expression forms;
- [ ] blocks/modules/structs: inspect and build staged surface containers where supported;
- [ ] patterns/types: expose only the subset needed before Stage 8 problem-aware macros, or document why they are deferred;
- [ ] source spans: preserve use-site spans where possible and expose enough span information for diagnostics.

### Hygiene Requirements

- [ ] identifiers built with introduction APIs are introduced at the macro-definition site;
- [ ] syntax values captured from macro input preserve their existing scopes and source spans where practical;
- [ ] introduced binders do not capture user references accidentally;
- [ ] caller-supplied binders bind in the output position where they are placed;
- [ ] explicit non-hygienic escapes are not added in Stage 7G unless there is a concrete tested need.

Deferred from Stage 7G until the macro system has all major phases implemented:

- user-facing macro diagnostic APIs beyond deterministic expansion errors;
- macro expansion debugging and tracing tools;
- recursion limits, cycle explanations, and expansion-control UX beyond existing deterministic failures.

### Tests

- [ ] hygienic builders can construct literals, variables, applications, lambdas, lets, and `do` blocks;
- [ ] preserved input syntax can be inserted into builder-created output unchanged;
- [ ] computed builders preserve hygiene for both introduced and use-site identifiers;
- [ ] syntax-object inspection can destructure a structured `syntax_operator_use` argument;
- [ ] a computed macro rewrites an enforested operator use without using only fixed pattern/template syntax;
- [ ] downstream-style macro examples use documented primitives rather than private implementation details.

### Exit Criteria

- [ ] At least one macro cannot be expressed cleanly with 7F pattern/template syntax alone;
- [ ] The public syntax-object API is documented and covered by tests;
- [ ] Public examples use `Syntax.*` rather than `stx_*` primitives;
- [ ] Existing 7F pattern/template examples remain simpler than their equivalent computed macro versions;
- [ ] Existing Stage 7 enforestation, hygiene, loader, and macro tests still pass.

## Phase 7I: Macros Generating Macros

Status: Planned. Phases 7F and 7H give macros the ability to produce expression and declaration templates. This phase extends that to allow macros to generate new macro, syntax, and operator declarations, so that macros can compose and build on each other.

### Purpose

Enable macros to produce `macro`, `syntax`, and `operator` declarations. This makes the macro system self-extending: a macro can define new compile-time behavior that is then available to later code in the same expansion scope. It relies on `$` being parsed as a separate token (decided in 7F), which keeps macro-expansion tokenization deterministic even when generated syntax contains holes.

### Scope

Add macro-generating-macro support with:

- [ ] a macro, syntax macro, or operator macro can produce a `macro`, `syntax`, or `operator infix` declaration in its expansion output;
- [ ] generated syntax declarations are registered in the operator environment during expansion so that later forms in the same block/module can use them;
- [ ] generated syntax declarations respect the same lexical scope, later-wins, and export rules as hand-written ones;
- [ ] `pub` on a generated macro, syntax, or operator declaration works like `pub` on a hand-written one;
- [ ] generated `pub` declarations are rejected deterministically in contexts that cannot export, such as local `do` blocks and ordinary runtime `struct ... end` expressions;
- [ ] generated computed macros (`macro` / `pub macro`) are phase-visited and elaborated through the same path as hand-written macros.

### Tests

- [ ] a syntax macro can generate another syntax macro and the generated syntax is usable later in the same expansion;
- [ ] a syntax macro can generate an `operator infix` declaration with correct precedence and associativity;
- [ ] a macro can generate another computed `macro` declaration and the generated macro is usable later in the same expansion;
- [ ] generated syntax declarations exported with `pub` are visible across module imports;
- [ ] generated `pub` declarations in non-exporting contexts are rejected deterministically;
- [ ] hygiene is preserved when generated syntax contains introduced identifiers;
- [ ] macro-generation cycles produce deterministic errors.

### Exit Criteria

- [ ] At least one macro-generated syntax form is tested end-to-end.
- [ ] Generated macro, syntax, and operator declarations compose with imported syntax extensions.
- [ ] Existing Stage 7 enforestation, hygiene, loader, and macro tests still pass.

## Remaining Stage 7 Work Tracker

These items are the resolved infrastructure debt for the original Stage 7 enforestation slice. Phases 7F through 7I above are now the remaining Stage 7 usability work before treating the macro surface as practically useful.

### Architecture

- [x] Generalize the current statement pre-scan decision: Stage 7 intentionally uses sequential declaration handling rather than a full Honu-style two-pass declaration pass because syntax extensions affect only later forms.
- [x] Revisit a real Honu-style two-pass declaration pass only if future syntax extensions need whole-block scope; decision: keep Stage 7 sequential declaration handling and defer a full two-pass pass until a future syntax-extension form actually needs whole-block scope:
  - pass 1 discovers value/type/module names and syntax-extension bindings for a block/module;
  - pass 2 enforests nested bodies with the completed binding and syntax-extension environment.
- [x] Replace the process-global dynamic operator refs in `Operator_env` with an explicit lexical operator environment threaded through enforestation.
- [x] Decide whether `Syntax_class.t` should become part of the parser API/context or be removed until Stage 8 needs it. Decision: keep it as parser/operator context data; operator lookup is syntax-class-aware while public entrypoints remain `parse_expr`, `parse_type`, and `parse_pat`.
- [x] Represent operator declarations as first-class data with fixity, precedence, associativity, syntax class, and expansion behavior instead of only prefix/infix helper registration.
- [x] Move more built-in operator handling into the same operator-table machinery used by syntax extensions where practical, especially arithmetic/comparison operators and assignment. Arithmetic/comparison and assignment now use operator declaration data; delimiter-driven postfix forms remain hard-coded for now.

### Syntax Extension Semantics

- [x] Define the Stage 7 user-facing syntax-extension declaration syntax as `operator prefix` / `operator infix`.
- [x] Specify shadowing and import precedence rules for multiple syntax extensions with the same symbol: later declarations/imports win deterministically.
- [x] Add deterministic ambiguity errors for incomparable syntax-extension candidates now that operator environments are lexical rather than global snapshots.
- [x] Support syntax-extension declarations that affect later forms in the same block/module with sequential operator-environment updates.
- [x] Decide whether syntax extensions can be defined for type, pattern, declaration, module-item, and block syntax classes before Stage 8 exposes problem-aware macros: Stage 7 exposes expression syntax extensions only.

### Macro Integration

- [x] Make syntax-extension expansion report useful source spans for the operator declaration and the use site.
- [x] Check that syntax-extension macro RHS values are expanded/evaluated through the same phase-aware path as `macro` / `pub macro` declarations in every supported module/block position.
- [x] Add negative tests for imported syntax-extension cycles that are distinct from runtime circular imports and ordinary macro visits.
- [x] Add tests for unsupported syntax-extension shapes so failures stay deterministic as the parser grows.

### Cleanup And Documentation

- [x] Update `SYNTAX_SPEC.md`, `IMPLEMENTATION_PLAN.md`, and `CLAUDE.md` so they agree on current comment syntax, Stage 7 status, and provisional syntax-extension forms.
- [x] Remove stale Menhir-related test dependencies if they are no longer needed by the semantic/backend test stanzas.
- [x] Document the current parser boundary: raw reader -> enforestation -> hygienic expansion -> lowering to `Surface.t`.
- [x] Keep `Surface.t` as the elaborator-facing boundary until Stage 8+ proves an interleaved expansion/elaboration design; decision: Stage 7 still lowers through `Surface.t`, with macro-only surface nodes preserved only for phase-aware expansion before elaboration.

## Compatibility Rules

- `Parse_expand.parse_expr` and `Parse_expand.parse_module` remain the only downstream entrypoints.
- Existing tests should not use a parser entrypoint below `Parse_expand` unless they are specifically testing the reader/enforester.
- New final syntax features should be added through enforestation.
- Lowering to `Surface.t` remains the elaborator-facing boundary for Stage 7.

## Error Handling Rules

Stage 7 should produce simple but deterministic errors. Good enough for this phase:

- unmatched delimiters report the delimiter and source span when available;
- unsupported forms report that they are outside the current enforestation slice;
- operator parse failures report the operator and expected operand position;
- unsupported redesigned forms should fail deterministically rather than silently choosing another parser.

## Implementation Notes

### Enforest Contract

The core function should consume exactly one form from a stream:

```text
enforest_expr : env -> min_prec -> raw_term list -> Syntax.t * raw_term list
```

It should follow the Honu/Pratt shape:

1. read a prefix/primary form;
2. repeatedly consume postfix and infix operators whose precedence is high enough;
3. return the grouped form and the unconsumed terms.

### Operator Environment

Represent built-ins as data where possible:

- name/token;
- fixity: prefix, infix, postfix, delimiter/postfix-call;
- precedence;
- associativity;
- builder function from syntax operands to `Syntax.t`.

Hard-coded branches are acceptable for complex first-token forms such as `fn`, `if`, `match`, and `do`, but arithmetic and comparison should use the table from the beginning.

### Syntax Classes

Use explicit syntax classes even before user-defined classes exist:

- `Expr`;
- `TypeExpr`;
- `Pattern`;
- `Decl`;
- `ModuleItem`;
- `Block`.

This keeps Stage 8 problem-aware macros from needing to reverse-engineer parser context later.

### Hygiene Boundary

Enforestation may create `Syntax.id` values with source spans, but it should not become the authoritative name-resolution pass. Scope introduction and resolution should remain in `Expand.expand` and `Expand_ctx`.

## Completed Initial Work Order

1. [x] Add `test/syntax/test_enforest.ml` with tests for Phase 7A shapes.
2. [x] Add raw reader support and make the reader tests pass.
3. [x] Add `Operator_env` and expression enforest support for literals, identifiers, parentheses, calls, fields, and operators.
4. [x] Route `Parse_expand.parse_expr` through enforestation.
5. [x] Add `do ... end` binding/final-expression support.
6. [x] Run `dune exec test/syntax/test_syntax.exe` and `dune build`.
7. [x] Update `IMPLEMENTATION_PLAN.md` Stage 7 status after Phase 7A and the later Stage 7 slices were implemented.

## Phase 7A Acceptance Checklist

- [x] `test/syntax/test_enforest.ml` exists and is included in `test/syntax/test_syntax.ml`.
- [x] `lib/expand/enforest.ml` exists.
- [x] `lib/expand/operator_env.ml` exists.
- [x] `lib/expand/syntax_class.ml` exists.
- [x] `Parse_expand.parse_expr` can parse the Phase 7A redesigned expression slice.
- [x] Parser-entrypoint users route through `Parse_expand`.
- [x] The old `core_parser.mly` parser is removed rather than extended for Phase 7A syntax.
- [x] `dune exec test/syntax/test_syntax.exe` passes.
- [x] `dune build` passes.
