---
title: "Port: references"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: references

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `ref e`, `deref r`, `r <- v` (the `<-` base role is merged, as a role with no
  elaboration yet), the `Ref(h, A)` type whose heap `h` is an implicit argument
  (surface `Ref(A)` takes a fresh one per use), and the base context's `Ref` and
  `Mutate` entries (see the prototype's `Elab_entry.stage1_ctx`).
- Refs in effect rows: an operation on a reference performs `Mutate(r)`; rows that
  name a parameter (`->{Mutate(r)}`, the effects fork's "rows that name their own
  parameter"); an unhandled `Mutate` names the reference; a `let` or block whose
  mutation is private discharges its local heaps (one pass over older metas).
- E6 through references: storing a closure that names a handled effect into a
  reference not local to the match is an error.
- Evaluation: a reference is a mutable cell; `ref`/`deref`/`<-` are machine
  frames, never native recursion.
- Rows on method results that mention a reference only if the effects follow-ups
  have not taken them (they are queued after this ticket): stop at method rows.

## Decided rules to read first

[refs-in-effect-rows](refs-in-effect-rows.md) (closed),
`docs/wayfinder/topics/core-tt-domain-model-effects.md`, the effects fork's merge
record in [port-effects](port-effects.md), `STATUS.md` "Effects follow-ups" (refs).

## Target

The 30 cases blocked on "the `ref` form", 3 on `deref`, 1 on `<-`, that need
nothing else unported.

## Resolution (2026-09-16)

Merged from `port/refs` (`44c2d8f`, `9b17c20`, merge `827f89d`, `4fd4b6a`).
`ref(e)`/`ref e`, `deref(r)`, `r <- e` (through the base role); base entries
`Ref : [h : Type] -> Type -> Type` and `Mutate : [h] -> [A] -> Ref(h, A) -> Type`, so
`->{Mutate(r)}` names a parameter. Every reference operation performs `Mutate(h)`;
each `ref` starts a fresh heap; an unhandled `Mutate` names a reference in scope on
that heap. At a lambda (infer and check), `let` and `rec let`, a local heap no
domain or result mentions is discharged; the entry discharges `Mutate` on any heap.
Stores are recorded on the sink and a match checks stores into non-local heaps for
escaping closures (E6). Reference forms are `Kont` frames over one `RefCell`.
`Unify.Mentions` (arrow rows, `VRefTy`) serves the occurs check and discharge.
Newly passing: elab-247, 249, 250, 258; macros core-258, 259; values core-025–028,
048, 049; shared `elaborate/ref-store-escapes-handler`,
`elaborate/ref-escaping-heap-not-pure`, `values/ref-local-heap-discharged` (agree
with the prototype). C# 229/661; xUnit 121.

**Follow-up (unverified deviation):** reading or writing a value whose type is an
unsolved meta unifies it with a fresh `Ref(?h, ?A)`; the prototype raises
`ApplyingNonFunction`. No case exercises it. Remaining ref cases need the prelude's `+`.
