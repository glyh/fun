# TODO

## Bugs (high)

### Disambiguate annotation names by scope

Currently `known_type_names` in `enforest.ml` is a hardcoded list:
`["I64"; "Bool"; "Unit"; "Char"; "String"; "Type"; "Id"; "Span"; "Expr";
  "Option"; "List"; "AtomVal"; "Explicitness"; "Ref"; "Pattern"; "Decl"; "Trait"]`

When parsing `: Expr(Foo)`, if `Foo` is in `known_type_names`, it becomes a
constraint (`Expr(None, Some "Foo")`). Otherwise, if it starts uppercase, it
becomes a type-binding parameter (`Expr(Some "Foo", None)`).

This means a user who defines `type MyTag = I64` cannot use `: Expr(MyTag)`
as a constraint — `MyTag` is treated as a binder. The disambiguation should
be context-sensitive, tracking an environment of type names in scope rather
than a static list. This applies (at least) to all 3 sites where the implicit
R-type parameter is constructed in `enforest.ml` (~lines 308, 316, 336).

### Nominal identity vs. name comparisons

Any code that compares nominals by `String.equal` on names is fragile because
nominals can be aliased through rebinds (`type T = SomeNominal`). The nominal's
`id` field exists for identity comparison and should be used instead of name
comparison wherever possible.

### Audit hardcoded symbol names

There are hardcoded string names scattered across the codebase — type names
(`"Type"`, `"R"`), constructor names (`"RExpr"`, `"Some"`, `"None"`), module
paths (`"Syntax"`), etc. These should be centralized or replaced with nominal
identity references where possible. Places to audit:

- `enforest.ml`: `"Syntax"`, `"R"` (R-type annotation), `"Type"` (old annotation)
- `elab_infer/elab_check.ml`: `"RExpr"` constructor name for wrapping
- `elab_resolve.ml`: `"stdlib"` module name
- `elab_prelude.ml`: stdlib source code has many hardcoded names
- `macro_eval.ml`, `expand.ml`, `expand_ctx.ml`: syntax nominal references

### Nested-module ADT constructor resolution

`pub pattern PatWild = RawPatWild(_)` inside modules can fail because constructor
resolution traverses by type name, not constructor name. Affects pattern matching
in macro bodies for module-scoped ADTs. The `find_nominal_template_opt` and
related code in `elab_patterns.ml` and `elab_resolve.ml` need attention.

### Private type visibility (design)

**Decision**: OCaml/SML path. Private types can leak through public bindings
but become abstract outside the module. Values can be passed around; constructors
are rejected unqualified.

**Surface syntax — sig integration**: `pub` annotations are the primitive;
a sig is an optional extract of `pub` entries that overrides inline annotations.

```fun
sig M = sig
  pub type R = RExpr(T) | RDecls | RPat
  pub value : I64                    -- type-only, no body
end

module M = do
  type Hidden = Wrap                 -- private (not in sig)
  type R = RExpr(T) | RDecls | RPat
  value = Wrap(1)
end
```

When a sig IS present, it restricts what's visible (only sig entries are public).
When absent, `pub` annotations on definitions determine visibility. The `pub`
syntax is identical in both — values in the sig use `:` instead of `=`.
Mechanical elevation, no parallel syntax.

**Unification**: `open_module_value` becomes the single access-control gate.
It registers both type names and their constructors (recursively for public
sub-modules). Private types never register → constructors naturally unreachable.
`unqualified_constructor_in_scope` can be dropped — the name table IS the check.

**Cross-language consensus** (10 languages): private constructors never usable
unqualified after `open`/`import`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
