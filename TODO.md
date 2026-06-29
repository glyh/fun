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

### Private constructor visibility (design decision)

Currently `elab_patterns.ml:239` checks `unqualified_constructor_in_scope` before
the VNominal constructor list. This correctly rejects constructors of private
ADTs when `open`-ed, but also rejects constructors of public ADTs nested inside
modules (e.g. `RExpr` from `Syntax.R`).

Cross-language research:
- **OCaml**: `.mli` controls visibility with 4 levels —
  *private* (not listed): type and constructors completely inaccessible;
  *abstract* (listed without `=`): type exists as opaque handle, constructors are
  unbound (can't create or match);
  *read-only* (`= private`): can pattern-match but can't construct directly;
  *public* (listed with `=`): constructors fully accessible. `open M` only
  exposes names from the public interface.
- **Java**: `private` members only accessible within declaring class.
  Package-private types returned by `public` methods become effectively opaque
  to external callers — values can be passed around but constructors/fields
  are inaccessible.
- **C#**: `private`/`internal` types can't be exposed through `public` members
  at all — the compiler rejects it ("type must be at least as accessible as
  the member"). `private protected`, `internal`, `protected internal` etc.
  are assembly-scoped.
- **Rust**: `use module::*` only brings `pub` items. Private struct fields
  can't be accessed outside the defining module.
- **Haskell**: export lists control constructor visibility. Importing a type
  without its constructors (`Foo` without `Foo(..)`) makes pattern matching
  impossible.
- **Swift/Kotlin**: constructors inherit the type's visibility.

Consensus: **private type constructors are never usable unqualified after open**.

Additional languages researched:
- **Standard ML**: Signatures (`sig ... end`) define interfaces; types not in the
  signature are *abstract* and constructors are hidden. `open` only exposes
  what's in the structure's signature. Opaque ascription (`:>`) hides
  implementation details.
- **F#**: `private` types "not usable outside this file". Discriminated union
  cases inherit the type's accessibility — a `private` DU has private
  constructors. Values "cannot be more accessible than their type" — no
  exposing a private type via a public binding.
- **Scala**: `private` restricts to enclosing class/object; `private[package]`
  to a package. `sealed` traits restrict inheritance to the same file — but
  case class pattern matching still requires the concrete type to be
  accessible. Default is `public`.
The current `unqualified_constructor_in_scope` check in `elab_patterns.ml:239`
is correct. The fix for `Syntax.R`'s constructors should be at the module-opening
level (recursive `open_module_value` registers public sub-module ADT constructors
into the name table). Do not weaken the constructor-visibility check.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
