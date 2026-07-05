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

### Stage 10 known limitation

Annotation-name disambiguation uses a static `known_type_names` list in the parser.
User-defined types (e.g. `type MyTag = I64`) are not recognized as constraints in
`Expr(MyTag)` — they are mistakenly treated as binders. This requires
expander/elaborator interleaving to fix and is the main open design issue before
Stages 11–12.

## Stages 11–12: Not specified

| Stage | Description | Status |
|-------|-------------|--------|
| 11 | Macro-powered language features | No spec |
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
  expander/elaborator interleaving needed to fix the Stage 10 annotation-name disambiguation
  limitation. Describes the required compiler shape, seams to replace, and regression tests.

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
- Imported macros are pre-compiled by `visit_macros` and cached in `macro_cache`.
- Type-aware macros (Stage 10) use `: Expr(A)` / `: Expr(_)` / `: Expr(I64)` annotations.
- The `Syntax.Expr` ADT with pattern synonyms provides computed macro authoring.
