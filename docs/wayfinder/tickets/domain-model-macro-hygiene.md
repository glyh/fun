---
title: Domain model — macro evaluation and hygiene
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: closed
assignee: glyh
resolution: Third pass done — model in ../topics/core-tt-domain-model-macros.md, vocabulary in the root CONTEXT.md (Reflection, Round trip, Provisional macro; Evaluation budget extended to count macro applications). Two decisions: reflection is total (the StxExpr escape hatch is scaffolding, not a boundary), and macro expansion is guarded by the one evaluation budget — a macro application is a call — retiring the depth fuel. The round trip's full loss inventory found and ticketed (the capture ticket is its scope slice); the type-aware site's silent-meta failure placed into its ticket; template heads placed inside the one hygiene contract; the dead `phase` field deleted. Suite green, 361 tests. All four passes of the port's specification are now done.
closed_date: 2026-09-13
blocked_by:
---

# Domain model — macro evaluation and hygiene

## Question

Third pass of the domain model: the seam pass two stopped at — *running*
macros. The port must carry what a macro is as an evaluated program and what
hygiene guarantees about what it writes. Today that is described by
`Macro_eval`'s wrap/unwrap and three divergent hygiene contracts (pass two's
three-paths table), not by named invariants:

- What is the **hygiene contract** of one macro or template application, stated
  as a single invariant — the two fresh scopes (intro, use-site), quoted ids
  resolving at the definition site, splices keeping their occurrence scopes,
  output expanded in place? Which parts hold on each of the three paths today,
  and what is the one contract the port enforces?
- What is the **round trip** — `Syntax.t` → value → `Syntax.t`? What must it
  preserve (scope sets — erased to a dummy `I64 0` by `id_to_value` on the way
  in, hardcoded `Scope_set.empty` by `value_to_id` on the way out: lost at both
  ends), what may it lose (spans — synthetic), what does it invent (`?` names)?
  Which forms are reflected (`Var`/`Atom`/`Ap`/`Lam`/`Let`) and what is the
  opaque `StxExpr` escape hatch *for* in the model, rather than as an
  implementation shortcut?
- What is a macro's **evaluation model**: body elaborated in its definition
  site's scope (today prelude-opened), run through one capability
  (`eval_and_apply`), recursion via provisional registration, under a **fuel**
  that guards application *nesting* (reserve/release around each application,
  256) — a different unit from the checker's call-counting evaluation budget.
  Do the two budgets keep separate names and units, and what does fuel
  exhaustion *be* — today it is a `failwith`, exception as control flow, not an
  error value naming the macro's caller?
- What replaces the **string fall-through** (pass two's S6)? Quoted syntax and
  borrowed context are the model's answers; what is the migration path for
  `Syntax.var("True")` reaching the prelude constructor, and what does the
  elaborator's flat second tier become when the first tier sees macro-written
  ids?
- Where does **type-aware output** get expanded? Lowering is only safe because
  expand already ran (S5), and the type-aware path never expands its output.
  The model must say where that expansion belongs so the skipped-expand defect
  is placed, not just patched.
- Are template **heads** part of the contract? Keyed by the string operator
  table at parse time (I4c), templates are the one path whose head resolution
  is not scope-aware — the same seam seen from the parse side. Does the one
  contract subsume scope-keyed template heads, or is that deliberately outside
  it?

## Why this is not tidying

Pass two's argument, compressed into what pass three actually touches:

- Hygiene is the port's highest-risk transliteration: a port that copies
  scope-set threading without the invariant passes every test until the first
  macro that writes a binder. The round trip erases scope sets at both ends
  today, and the capture defect is already diagnosed — a transliteration
  reproduces it with new spelling.
- The two-tier resolution is load-bearing: porting it away breaks every macro
  that writes a prelude name; porting it silently inherits the leak
  ([block-local-macros-leak-by-written-name](block-local-macros-leak-by-written-name.md)
  is its macro-table twin).
- Fuel and the evaluation budget have different units (nesting depth vs
  calls + iterations) and different failure behaviour (`failwith` vs a compile
  error naming the call). A port that merges them gets runaway or over-strict
  expansion; a port that copies the `failwith` inherits exception-as-control-
  flow at the one boundary where the error should be a value.

## Scope

In: `Macro_eval` (wrap/unwrap, `syntax_nominals`, the reflection ADTs), the
three application sites (`Expand`'s macro call, `enforest_template`'s template
application, `Macro_driver`'s type-aware path), fuel and provisional macros,
quoted syntax and borrowed context, the string fall-through (S6) with its
macro-table twin, and template head keying.

Not this pass: enforestation and the reader (pass two), effects (pass four —
[done](../topics/core-tt-domain-model-effects.md)), the `Surface.t` merge
decision ([delete-surface-ir](delete-surface-ir.md)), and *implementing* the
hygiene fixes — the defect tickets exist; this pass places them in the model.

## Deliverables

1. A topic doc under `topics/` naming the concepts and invariants of macro
   evaluation and hygiene, each marked enforced / checked / unchecked.
2. `CONTEXT.md` vocabulary for those layers.
3. Defects and misnamed concepts spun out as their own tickets; corrections to
   the existing defect tickets where the model re-frames them.

## Resolution

**Done.** Model in
[core-tt-domain-model-macros](../topics/core-tt-domain-model-macros.md),
vocabulary in the root [`CONTEXT.md`](../../../CONTEXT.md). Eight invariants
named and classified (M1–M8).

Two decisions were made in the pass, both grilling-resolved:

- **Reflection is total.** The ADTs cover the whole grammar; nothing is
  opaque; the `StxExpr` escape hatch is scaffolding until they do, not a
  boundary. This makes the round-trip question sharp: destructuring followed
  by reconstruction must be the identity — and today it is lossy in both
  directions.
- **One evaluation budget.** A macro application is a call the checker
  evaluates, so it counts under the same budget the effects pass designed —
  calls, iterations, applications; one unit, one error, raisable. The
  Racket/Klister-style depth fuel retires: it cannot catch breadth blowup
  (bounded depth, unbounded sibling spawning), and its `failwith` delivery is
  exception-as-control-flow at the boundary that most wants error values.
  [checker-evaluation-budget](checker-evaluation-budget.md) is extended
  accordingly.

The pass found more than it was asked to model:

- **The round trip's full loss inventory** — scope sets at both ends (the
  wrap side writes a dummy `I64 0`; the unwrap side does not read the field),
  type annotations dropped on reconstruction, `Ap` explicitness hardcoded
  back to `Explicit`, `PatCon` paths destroyed by the wrap itself, span
  line/col discarded, and four silent degradations (`?` ids,
  garbage→`False` in `value_to_bool`, skipped non-`Let` bindings,
  filtered pattern lists). Spun out as
  [syntax-round-trip-is-lossy](syntax-round-trip-is-lossy.md), with
  [procedural-macros-capture-use-site-variables](procedural-macros-capture-use-site-variables.md)
  as its soundness slice.
- **A silent failure at the type-aware site**: when the macro's result fails
  to unwrap as syntax, the elaborator mints a fresh meta — a failed expansion
  becomes an unsolved hole. Placed into
  [type-aware-macro-output-is-not-expanded](type-aware-macro-output-is-not-expanded.md),
  whose helper should fix both.
- **Template heads belong inside the one hygiene contract** (M7): operators
  and templates are binders with syntactic roles, resolved by scope set —
  pass one's I4c seam, now placed rather than merely deferred.
- **`Expand_ctx.phase` was dead** — `Runtime` constructed once, copied once,
  never read, `CompileTime` never constructed. Deleted (pass one's dead
  `types` column precedent). Suite green, 361 tests.

With this, **all four passes of the port's specification are done**: the
elaborate ↔ evaluate boundary, surface and enforestation, macro evaluation
and hygiene, and effects. What remains between the prototype and the port are
the defect tickets — each an invariant's distance — no longer unnamed rules.
