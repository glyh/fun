---
title: The elaborator's macro runtime is still a mutable field
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: `Ctx.macro_runtime` is immutable. A context gets a runtime by construction (`Ctx.with_expander ctx ectx`) at every former write site (macro driver, import, `bin/main.ml`, test helpers); the import site builds `with_expander (unit_base ctx) expand_ctx`, a new record, so even when `unit_base` returns the importer's own context (`base = None`) the importer's handle is untouched. No dedicated test - the overwrite is no longer expressible, and a `base = None` context cannot import (import needs the prelude's syntax nominals).
assignee:
blocked_by:
---

# The elaborator's macro runtime is still a mutable field

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariant

**I4e**: the elaborator's expander handle is a capability passed with the
context, not state that is reached into and overwritten.

## Where the code deviates

- `Ctx.macro_runtime` is `mutable` (`lib/semantic/typecheck/elab_ctx.ml:33`),
  written at `macro_driver.ml:52` and `elab_infer.ml:535`.
- The import site copies first (`Ctx.unit_base`), but `unit_base` returns the
  importer's *own* record when `base = None` (`elab_ctx.ml:87-89`). The write at
  `elab_infer.ml:535` then overwrites the importer's handle: last writer wins, the
  latch the closed `expander-handle-is-a-capability-not-a-context` ticket removed
  in name.

## Direction

Make the field immutable and set it by record construction at both sites. Make
`base = None` impossible after `init_ctx`, or copy in that branch too.
