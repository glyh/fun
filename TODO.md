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

**Surface syntax — sig integration**:

Two mutually exclusive modes. A module either uses inline `pub` annotations
OR a sig — never both.

```fun
-- Mode A: inline pub
module M = do
  type Hidden = Wrap            -- private (no pub)
  pub type R = RExpr(T) | RDecls | RPat
  pub value = Wrap(1)
end

-- Mode B: sig — no pub annotations in the body
sig M = sig
  type R = RExpr(T) | RDecls | RPat
  value : I64                  -- body becomes type annotation only
end

module M : M = do
  type Hidden = Wrap            -- everything private by default
  type R = RExpr(T) | RDecls | RPat
  value = Wrap(1)
end
```

When a sig is present, `pub` in the module body is a compile error.
The `pub` syntax is identical in both — values in the sig use `:` instead of `=`.
Elevation from inline pub to sig is mechanical: copy the `pub` entries from the
body into a sig block, replace `=` with `:` for values, drop `pub` from the body.

**Unification**: `open_module_value` becomes the single access-control gate.
It registers both type names and their constructors (recursively for public
sub-modules). Private types never register → constructors naturally unreachable.
`unqualified_constructor_in_scope` can be dropped — the name table IS the check.

**Cross-language consensus** (10 languages): private constructors never usable
unqualified after `open`/`import`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
