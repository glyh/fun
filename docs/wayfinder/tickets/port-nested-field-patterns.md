---
title: "Port: a nested field pattern works — the hang this ticket recorded was a different bug"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 as a mis-diagnosis. Nested field patterns already work (VALUE 5, probe below); the hang this ticket recorded came from a comma in the struct declaration, which is now its own ticket (port-reader-loops-on-struct-field-comma), and the one shape that does fail is the atom field pattern (port-atom-field-pattern-misunifies). The ruling that "a nested field pattern is supported" therefore required no work.
assignee:
blocked_by:
---

# Port: a nested field pattern works — the hang this ticket recorded was a different bug

> ## Resolution (2026-09-25) — closed as a mis-diagnosis
>
> **The feature is already there.** With the program written in the language's own syntax — which
> the probes on this ticket were not — a nested field pattern simply works:
>
> ```fun
> { R = struct { f : I64; g : Option(I64) };
>   f = fn(x : Option(I64), y : match (R{f = 1; g = x}) { R{f = 1; g = Some(z)} => I64, _ => Char }) { y };
>   f(Some(5), 5) }
> ```
>
> → `VALUE 5`, as it should be, with the bare-binder control (`g = y`) also `VALUE 5`. So the
> ruling this ticket carried — *a nested field pattern is supported, support it rather than
> refusing it* — describes behaviour that already existed. No work was owed.
>
> **What the ticket's "hang" actually was.** Every probe on this ticket declared the struct as
> `R = struct { f : I64, g : Option(I64) }` — with a **comma** — and the port's reader loops
> forever on a comma in a struct field list:
>
> ```fun
> { R = struct { f : I64, g : I64 }; 1 }      -- one field, or `;`, answers VALUE 1
> ```
>
> So the hang happened in the **reader**, before any pattern was parsed or elaborated, and the
> "three programs pinning down the field-pattern path" that this ticket was built on were all
> pins on the same comma. It is now its own ticket,
> [the reader loops forever on a comma in a struct field list](port-reader-loops-on-struct-field-comma.md),
> with the debugger stack and the resource profile that identify it.
>
> **The one shape that really does fail** — found only after the syntax was corrected — is the
> atom field pattern, `R{f = 1; g = 2}` against `g : I64`, which reports
> `cannot unify VAtomTy with VAtomTy` instead of matching:
> [an atom pattern in a record field does not unify](port-atom-field-pattern-misunifies.md).
> That is where the field-pattern work actually is.
>
> **The lesson, which cost three separate corrections in one session:** a probe is evidence only
> if it is a *valid program*, and "invalid" here was not a subtlety — it was a comma and a bare
> `{…}` where the language spells `;` and `R{…}`. Every one of those invalid probes produced a
> confident conclusion that survived review, because a hang looks like a hang whether or not the
> program parses. Check the shape against a passing conformance case before believing a result
> that "the language has a hole here".
