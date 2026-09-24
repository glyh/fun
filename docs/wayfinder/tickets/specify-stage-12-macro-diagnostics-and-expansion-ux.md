---
title: Stage 12 macro diagnostics / expansion UX spec
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
  - design-type-aware-macro-interleaving.md
---

# Stage 12 macro diagnostics / expansion UX spec

## Question

Specify the Stage 12 diagnostics/expansion UX scope, and decide what belongs
before the CLR / C# rewrite.

## Context

- The roadmap says broad diagnostics polish is post-rewrite.
- Pre-rewrite diagnostics work should be scoped narrowly — only enough to
  unblock macro feature work and macro-level tests.
- Items like structured error spans, traceable expansion output, and
  user-facing macro error messages need a clear boundary.

## Resolution

_Unresolved._

## Deferred to after the .NET port (2026-09-15)

- **A typed macro call whose expected type contradicts its promise** fails today
  with a plain unification error before the macro runs (`b : Bool = n()` with
  `macro n() : Expr(I64)` → "cannot unify I64 with Bool"). Preferred message
  (user, 2026-09-15): "macro `n` promises Expr(I64), but Bool is expected here".
  Diagnostics polish — do after the port.

## Input from the divergence review (2026-09-21)

The review of the four "prototype-only bugs" found a rule that needs a diagnostic which is
**not** an error, and neither implementation has a channel for it.

- **An unguarded recursive occurrence is uninhabited.** `rec L = struct { v : I64; next : L }`
  has no finite value — instantiating it needs infinite memory — so it is worth a
  **warning**, not an error: the *type* stays useful for type-level work, and an infinite
  structure can still be built through a `Ref`. A recursive occurrence is **guarded** when it
  sits under a former that can be inhabited without it — a sum (`Option(L)`), an arrow
  (`Unit -> L`) or a `Ref(L)`; records and tuples do not shelter. Measured: the prototype
  accepts every shelter form above *and* the mutual unguarded pair
  (`rec A = struct { b : B } and B = struct { a : A }`), so its rejection of the direct
  self-field (`values/rec-record-field-of-own-type`) is a `CannotUnify(struct type vs Type)`
  accident rather than a rule — the case stands as `desired` (the port accepts it, `1`).
- **What this asks of this ticket**: where non-fatal diagnostics live (`Elab_error` /
  `FunException` are errors only), how they reach the user (REPL, loader, driver), and
  whether the shared conformance runner can assert one — its `.expect` expresses only `ok`,
  `error`, or a value, so a warning covered by an xUnit test alone is invisible to
  `test/conformance` and joins the invisible delta the parity work exists to close.
- Related, tracked separately: the same review's decision 3, that a container's public
  members are unique, is an *error* and needs no new channel
  ([a module's public members are unique](public-members-are-unique.md)).
