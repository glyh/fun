# Macro status — canonical

This is the **authoritative** macro-system status document. All other macro docs
defer to this file for completion status.

---

## Stages 0–10: Complete / implemented

| Stage | Description | Status |
|-------|-------------|--------|
| 0 | Baseline regression lock | Done |
| 1 | Source spans and raw syntax | Done |
| 2 | Syntax objects and scope sets | Done |
| 3 | Built-in hygienic expander → Surface.t | Done |
| 4 | Move name introduction into expansion | Done |
| 5 | Minimal local macro definitions | Done |
| 6 | Phase-aware module loading | Done |
| 7 | Enforestation and regular syntax extension | Done (all sub-phases 7A–7I) |
| 8 | Kind-tagged macros | Done |
| 9 | Decl/Pattern ADT completion | Done |
| 10 | Type-aware macros | Done |

### Stage 10 annotation disambiguation — resolved on the driver path

The old static `known_type_names` parser list has been removed. Stages 1–9 of
the type-aware interleaving migration are complete: the expander's
`MacroBinding` site uses an injected `resolve_macro_kind` callback (set by
`Macro_driver.run`) for canonical semantic kind resolution at registration
time; generated declarations re-enter the expander; scoped per-binding
semantic advancement makes top-level source-order prior declarations
(builtin types, user type/record declarations, and value aliases such as
`MyInt = I64`) constrain later macro annotations, while unresolved uppercase
names remain binders and lowercase/wildcard are unconstrained. Qualified
annotations (`: Expr(M.T)`) resolve against imported module types. The
recursive-macro safety slice is implemented: top-level macro definitions use
provisional registration with rollback, and macro expansion has a depth-style
fuel guard shared across copied expand contexts. Stage 8 replaced the old
`Core_loader.visit_macros` import path with `Macro_driver.visit_macros`, which
compiles an imported module's public macros through a full driver run so
their annotations resolve in the imported module's own advancing context;
Stage 9 retired the old loader path.

Still deferred: same-Decl generated type→macro interleaving,
transformer-level self-recursive macro bodies, mutually recursive macro
groups, semantic `resolved_type_ref` constraint identity, and
resolved-export cache fingerprinting. The final full queue driver must avoid
double-elaboration/nominal freshness drift.

## Stages 11–12: Not specified

| Stage | Description | Status |
|-------|-------------|--------|
| 11 | Macro-powered language features | In progress — direction = demote built-in constructs to library. Increment 1 (Bool ADT + `if`⇒`match`, `Core.If`/`FIf` removed) done, 778 tests green. See [Bool and `if` as library features](../topics/bool-and-if-as-library.md). |
| 12 | Macro diagnostics & expansion UX | No spec |

## Key documents

- **[IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md)** — Full staged implementation plan with
  detailed checklists and design invariants. Contains a mix of current-status checkmarks
  (accurate) and prose that may refer to "not started" or "pending" which was accurate
  at the time of writing but is now stale. **Refer to this STATUS.md for actual completion status.**

- **[SUMMARY.md](SUMMARY.md)** — Design summary: goals, source influences, core principles
  (syntax objects, hygiene by scope sets, regular syntax via enforestation,
  first-class compile-time macros, problem-aware expansion, type-aware/type-providing macros,
  stuck macros and interleaving, module/phase implications, compiler structure impact).
  The "Suggested implementation path" section reflects the original plan and may not
  match current completion status.

- **[TYPE_AWARE_INTERLEAVING.md](TYPE_AWARE_INTERLEAVING.md)** — Design document for the
  expander/elaborator interleaving that fixed the Stage 10 annotation-name disambiguation
  limitation. Stages 1–9 are implemented, including semantic kind resolution,
  generated declaration re-entry, per-binding semantic advancement, provisional
  macro registration/rollback, depth-style macro fuel, and driver-based import
  loading. Describes the long-term queue-driver compiler shape.

- **[STAGE_7_ENFORESTATION_PLAN.md](STAGE_7_ENFORESTATION_PLAN.md)** — Concrete implementation
  plan for Stage 7 enforestation (phases 7A–7I). All sub-phases are complete. The document
  remains a useful architectural reference for the enforestation design.

- **[STAGE_8_PLAN.md](STAGE_8_PLAN.md)** — Original design document for Stage 8 kind-tagged
  macros. The document's status markers ("Not started") are stale; Stage 8 is complete.
  The design decisions (kind inference from body, name shadowing, no implicit problem
  parameter) are accurate.

- **[INDEX.md](INDEX.md)** — Reference catalog: papers, extracted Klister commentary
  and examples.

## Implementation notes

- The expander uses `Expand_ctx.t` for macro processing during parsing, requiring
  `elaborate` and `eval_and_apply` callbacks.
- The elaborator uses `Elaborate.Ctx.t` for `Surface.t → Core.term`.
- Imported macros are pre-compiled by `Macro_driver.visit_macros` (a full
  driver run over the imported module) and cached in the loader's `macro_cache`.
- Type-aware macros (Stage 10) use `: Expr(A)` / `: Expr(_)` / `: Expr(I64)` annotations.
- The `Syntax.Expr` ADT with pattern synonyms provides computed macro authoring.
