# Stage 7 — self-recursive macro cells + fuel guards

## Goal

Implement the next type-aware macro interleaving migration step from
`docs/17.type_aware_macro_interleaving_design.md`: self-recursive macro
definitions with safe provisional registration and bounded expansion fuel.

## Current status

- Previous commit: `89df97a` (`Stage 7: scoped per-binding semantic advancement`).
- That commit added per-binding semantic advancement in `Macro_driver.run`, but
  did not implement the design-doc Stage 7 item: self-recursive cells + fuel guards.
- Working tree was clean after the commit.

## Intended semantics draft

- When compiling a top-level macro definition, register a provisional macro cell
  before elaborating/evaluating its body so the macro can refer to itself by name.
- Fill the cell atomically after successful compilation/evaluation.
- Roll back provisional registration on compilation/evaluation failure.
- Reject macro expansion through an unfilled provisional cell with a clear
  diagnostic unless the design supports delayed self-reference safely.
- Add expansion fuel to prevent runaway recursive macro expansion.
- Preserve existing ordering: prior bindings are visible, later bindings are not.

## Research / review log

- `exp-1` / `ses_0c7d855ccffeGvAKj21E1bKUsz` (`@explorer`) launched to map
  macro registration, lookup/storage, existing recursion guards, and likely tests.
- `exp-1` reconciled. Confirmed there is currently no expansion fuel and no
  provisional macro-entry state. Current relevant seams:
  - `lib/expand/expand_ctx.ml`: `macro_entry = { value; syntax_nominals }`,
    `macro_table`, `macro_kind_table`, registration/lookup helpers.
  - `lib/expand/expand.ml`: expression `MacroDef`, expression `MacroCall`,
    operator macro expansion, `expand_struct_bindings_with_scopes`, top-level
    `MacroBinding`, and `MacroCallBinding` declaration expansion.
  - `lib/semantic/typecheck/elab_infer.ml` / `elab_check.ml`: type-aware macro
    expansion can occur during elaboration through `ctx.expand_ctx`; this path
    also needs fuel if fuel is meant to guard all macro expansion.
  - `lib/semantic/typecheck/macro_driver.ml`: driver builds one `expand_ctx`,
    injects callbacks, advances `elab_ctx` per binding, then copies ready macros
    into `elab_ctx.macro_table` after expansion.
- `Core.value` has `VRef of value ref`, but macro lookup/application currently
  expects a direct callable `Core.value`; there is no existing macro-cell
  abstraction.

## Candidate implementation seams to confirm

- `lib/semantic/typecheck/macro_driver.ml`: canonical driver registration path.
- `lib/expand/expand_ctx.ml`: macro table storage and kind table.
- `lib/expand/expand.ml`: macro invocation expansion path and likely fuel hook.
- `lib/loader/core_loader.ml`: legacy import/visit paths that may need wrapping
  but should not be fully replaced until the later driver-based import stage.
- `test/backend/test_core.ml`: likely macro driver regression tests.

## Open questions for oracle review

1. Should provisional cells live directly in `Expand_ctx.macro_table`, or should
   `Expand_ctx` expose a new macro-entry state (`Pending`/`Ready`) around values?
2. Should expansion fuel be stored in `Expand_ctx.t`, threaded explicitly through
   expansion functions, or scoped only in `Macro_driver.run`?
3. What exact behavior should self-recursive macros have at definition time when
   their own body expands themselves during compilation?
4. How much should legacy import/visit paths be adapted now versus deferred to
   the driver-based import stage?

## Draft plan for oracle review

### A. Fuel guard

1. Add shared expansion fuel to `Expand_ctx.t`:
   - `macro_fuel_limit : int`;
   - `macro_fuel : int ref` so `Expand_ctx.copy` shares the same fuel budget
     across recursive/scoped expansion copies instead of resetting it.
2. Add helper functions in `Expand_ctx`:
   - `default_macro_fuel_limit` (likely `256` per design doc);
   - `consume_macro_fuel ctx ~site ~name` that decrements or fails with
     `macro expansion exceeded fuel limit`.
3. Consume fuel at every ready macro application site:
   - `Expand.expand`: expression `MacroCall` before `eval_and_apply`;
   - `Expand.expand`: `SyntaxOperatorUse` before `eval_and_apply`;
   - `Expand.expand_struct_binding`: `MacroCallBinding` before `eval_and_apply`;
   - `Elab_infer`/`Elab_check`: type-aware macro expansion path before applying
     a macro from `ctx.macro_table`, using `ctx.expand_ctx` to share budget.
4. Do not consume fuel when a call is not resolved to a macro or when a typed
   expression macro is deliberately deferred from expander to elaborator; consume
   at the actual application site.

### B. Provisional self-recursive macro registration

1. Extend `Expand_ctx` macro storage with explicit state rather than stuffing a
   fake `Core.value` into a ready entry:
   - ready entry: current `{ value; syntax_nominals }`;
   - provisional entry: records at least `syntax_nominals` and maybe origin/name.
2. Preserve existing lookup API for callers that need callable macros:
   - `lookup_macro_entry` should return only ready entries;
   - add `lookup_macro_state` if code needs to detect/report provisional entries.
3. Add registration helpers:
   - `snapshot_macro ctx name` and `restore_macro_snapshot ctx name snapshot`;
   - `register_provisional_macro ctx ~name ~syntax_nominals`;
   - `fill_provisional_macro ctx ~name ~value`.
4. In top-level `MacroBinding` processing, resolve kind and introduce the macro
   name/provisional kind before compiling the macro value, then fill on success
   and restore/remove on failure. The exact point of binding-scope introduction
   is the key uncertainty: current code extends the macro name after compiling;
   self-recursion may require extending before expansion and possibly expanding
   the value under the macro's own scope.
5. Apply analogous provisional logic to expression-level `MacroDef` only if the
   semantics require local self-recursive macros; otherwise limit this stage to
   top-level module `MacroBinding` and document/defer `MacroDef`.

### C. Risk to review

- A provisional entry that is visible to ordinary macro-call lookup must not be
  applied as a real macro. If a macro body actually forces its own provisional
  macro during definition, this should produce a clear diagnostic rather than a
  bogus value or infinite loop.
- The elaborator macro tables (`Elab_ctx.Ctx.macro_table`) store only ready
  `(Core.value * MacroKind.t * nominals)` tuples. If self-recursive macro bodies
  genuinely need elaborator-level self lookup during compilation, `Elab_ctx` may
  also need a stateful macro-entry abstraction. Avoid that unless tests/semantics
  prove it is necessary for this stage.
- Existing old paths (`visit_macros`, runtime parser helpers, imported macro
  cache) should receive fuel where they apply macros through `Expand_ctx`, but
  should not be fully rewritten until the driver-based import stage.

## Validation plan

- Targeted macro driver tests for:
  - self-reference is registered before macro body compilation;
  - failure rolls back provisional entry;
  - runaway recursive expansion fails via fuel/diagnostic;
  - existing macro tests still pass.
- Full `dune test` before commit.

## Oracle review resolution

- Accepted oracle's scope recommendation:
  - provisional cells are top-level `MacroBinding` infrastructure only;
  - supported recursion model is re-expansion recursion, not transformer-level
    self-application during a macro's own definition;
  - fuel is global/shared across all real macro application sites.
- Fuel is consumed at actual apply sites only:
  - non-typed expression macro calls in `lib/expand/expand.ml`;
  - syntax operator macro calls in `lib/expand/expand.ml`;
  - declaration macro calls in `lib/expand/expand.ml`;
  - typed macro expansion in `lib/semantic/typecheck/elab_infer.ml`;
  - typed macro checking in `lib/semantic/typecheck/elab_check.ml`.
- `Expand_ctx.copy` shares the fuel ref so copied/scoped contexts draw from the
  same budget.
- Provisional registration uses snapshot/restore so a failing duplicate macro
  definition restores the previous ready macro and kind.

## Implementation status

- Implemented `Expand_ctx` fuel and provisional helpers.
- Restructured top-level `MacroBinding` expansion to register provisional before
  compilation and fill/restore around compilation.
- Added `test/backend/test_macro_driver_stage7.ml` because `test_core.ml` is at
  2993 lines and the 3000-line limit is strict.
- Added 5 focused tests:
  - expression macro fuel exhaustion;
  - expression macro one-budget success;
  - declaration macro fuel exhaustion;
  - driver provisional fill/clear;
  - provisional rollback restores previous macro/kind.

## Validation results

- `dune exec test/backend/test_macro_driver_stage7.exe` — OK, 5 tests.
- `dune exec test/backend/test_core.exe` — OK, 321 tests.
- `dune test --force` — OK.

## Code review reconciliation

- Oracle flagged the first fuel implementation as a module-wide monotonic total
  cap. Reworked fuel into depth-style guard: reserve on macro expansion entry,
  release on exit, while preserving the shared ref through `Expand_ctx.copy`.
- Replaced direct `consume_macro_fuel` call sites with `Expand_ctx.with_macro_fuel`
  so recursive re-expansion consumes nested budget but sibling macro calls do not
  accumulate against one module-wide total.
- Added provisional-call diagnostics for pending macro names that are encountered
  at macro-call sites during their own definition.
- Removed unused/inconsistent provisional helper paths and redundant driver reset.
- Fixed typed check fallback to avoid re-entering macro inference after a
  transformer already ran and returned a non-syntax value.
- Added 2 more tests:
  - shared fuel ref across `Expand_ctx.copy`;
  - fresh-definition failure rollback removes ready entry, kind, and pending marker.

## Final validation after review fixes

- `dune exec test/backend/test_macro_driver_stage7.exe` — OK, 7 tests.
- `dune exec test/backend/test_core.exe` — OK, 321 tests.
- `dune test --force` — OK.
