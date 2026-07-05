# Wayfinder Map — Compiler Directions

> **Local-tracker note:** This repo has no `docs/agents/issue-tracker.md`. This map
> uses local Markdown. Ticket titles are names and also serve as links to ticket
> files.

## Notes

- Canonical status docs are **`docs/STATUS.md`** and **`docs/plan-for-macros/STATUS.md`**.
  When other docs disagree, STATUS.md wins.
- Prefer `ocaml-alcotest` for test/debug sessions.
- Macro work should consult **`docs/plan-for-macros/TYPE_AWARE_INTERLEAVING.md`**.
- Do not trust stale completion prose in macro implementation plans when STATUS
  files disagree.

## Decisions so far

- **01 — Type-aware macro interleaving handshake** (resolved): design-only;
  semantic module driver with explicit queue, split annotation/kinds, ordered
  pass, macro_exports, staged migration. See
  [design doc](../../17.type_aware_macro_interleaving_design.md).

## Fog

Areas that are dim or underspecified and will need attention later:

- **CLR / C# rewrite shape** — what the production rewrite target looks like,
  how much of the prototype maps directly, and what changes structurally.
- **Surface syntax after macro model settles** — whether broad surface syntax
  should change once the macro expansion model is finalised.
- **Diagnostics polish boundary** — what diagnostics work belongs pre-rewrite
  (only to unblock macro feature work/tests) vs post-rewrite (broad polish).
- **Library-level features vs compiler machinery** — how much of future feature
  work should be library-level macros / type-case rather than new compiler
  machinery.

## Open tickets

Intended frontier order. Each item is an index entry only — decisions live in
the ticket files.

1. [Type-aware macro interleaving handshake](tickets/01-type-aware-macro-interleaving.md)
   — **resolved/closed** — design doc at `docs/17.type_aware_macro_interleaving_design.md`.
2. [Stage 11 macro-powered language features spec](tickets/02-stage-11-macro-powered-language-features-spec.md)
   — **open** — blockers: none; what Stage 11 should include.
3. [Stage 12 macro diagnostics / expansion UX spec](tickets/03-stage-12-macro-diagnostics-expansion-ux-spec.md)
   — **open** — blockers: none; diagnostics scope pre-rewrite.
4. [Trait library deriving and protocols](tickets/04-trait-library-deriving-and-protocols.md)
   — **open** — blockers: none; library-level deriving/protocol ops for traits.
5. [Private type visibility model](tickets/05-private-type-visibility-model.md)
   — **open** — blockers: none; decision on private/opaque type visibility.
6. [Generated symbol cleanup scope](tickets/06-generated-symbol-cleanup-scope.md)
   — **open** — blockers: none; when to do generated-symbol cleanup.
7. [Enforester improvements scope](tickets/07-enforester-improvements-scope.md)
   — **open** — blockers: none; which enforester improvements to do pre-rewrite.
