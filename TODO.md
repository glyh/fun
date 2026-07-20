# TODO

For detailed current implementation status, see [`docs/STATUS.md`](docs/STATUS.md).

## Bugs (high)

### Disambiguate annotation names by scope — RESOLVED

All nine stages of the type-aware interleaving migration are complete.
Macro annotations are resolved semantically against the current prior type
namespace on the driver path: builtin types, prior user type/record
declarations, and value aliases (`MyInt = I64`) constrain; qualified
annotations (`: Expr(M.T)`) resolve against imported module types;
unresolved uppercase names remain binders; lowercase/wildcard are
unconstrained. Imported macro modules are compiled through
`Macro_driver.visit_macros` (a full driver run over the imported module),
so their annotations resolve in the imported module's own advancing
context; the old `Core_loader.visit_macros` parser-heuristic path is
retired.

Deferred follow-ups (design gates for the full queue driver, tracked in
`docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md` and
`docs/17.type_aware_macro_interleaving_design.md`):

- Same-Decl generated type→macro interleaving (a macro-generated type does
  not yet constrain a macro generated later in the same Decl output).
- Transformer-level self-recursive macro bodies and mutually recursive
  macro groups.
- Semantic `resolved_type_ref` constraint identity (constraints are still
  recorded by name and resolved at the use site).
- Resolved-export cache fingerprinting (the loader macro cache is still
  keyed by module path, not resolved export identity).
- Making the queue driver the single main pipeline (the REPL expression
  path still uses `Parse_expand` with the parser-side annotation adapter
  for expression-level macro definitions).

### Private type visibility (design)

**Status**: separate task. Do not implement as part of the current concrete bug
fix pass. Track the design in `docs/16.private_type_visibility.md`.

## Features (medium)

### Stage 11: Macro-Powered Language Features (no spec yet)

### Stage 12: Macro Diagnostics & Expansion UX (no spec yet)
