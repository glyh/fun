---
title: "Port: a recursive enum's captures over-capture enclosing names"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a recursive enum's captures over-capture enclosing names

Reported by the fork that fixed
[captures come from payload values](port-enum-captures-from-payload-values.md)
(2026-09-24) — it saw this, judged it pre-existing and identity-consistent, and left it
alone rather than widening its own change. Recorded here so it is not dropped.

**There is no reproducing program yet. That is this ticket's first task**, and the reason
this is not written up as a defect: if nothing observable fails, there is nothing to fix
and no case to add. The reporter's description — "a value binder named in the body", e.g.

```text
an enclosing value binding referred to by a member of a rec enum group
```

— is a direction, not a reproduction. **Do not treat it as one.**

## What is known

- The port computes a member's captures as *the enclosing body's names ∪ the levels quoted
  out of its payload values* (`EnumCaptureLevels`, quoted through `Nbe.Quote`), whereas the
  prototype's `capture_payloads` / `enclosing_scope` pair does not admit the enclosing names
  the same way. The port therefore captures **more**.
- Captures are what E11 identity compares, so an observable form would look like two
  nominals the port treats as distinct where the prototype treats them as the same, or the
  other way round — reached through a **type-case**, a signature check, or the
  unrelated-records-cannot-unify comparison. That is the shape to hunt for.
- The difference is *not* reachable through the cases the merged ticket added (those are
  about payload values, and both runners agree on them).

## First task

1. Build the smallest program that observes a difference in captures between the two
   implementations, or establish honestly that none exists. Probe both runners — the
   prototype is a *map*, not the spec, and this area has already produced causes that
   probing overturned.
2. If a program exists: decide which side is right against the domain model (a **prototype
   defect** fixed in C# only, per convention 5, is a live possibility — "the port captures
   more" is not automatically the bug). Fix accordingly and add a shared case, ordinary if
   the prototype is right and listed in `prototype-divergences.txt` if it is not.
3. If no program exists, **close this ticket saying so** and record the difference as
   unobservable — an honest negative result is the wanted outcome, and it retires the claim
   rather than leaving it to be re-discovered.

## Reading

- [captures come from payload values](port-enum-captures-from-payload-values.md) — its
  Resolution describes the helper, the fixed point, and the name-based seed
- `dotnet/src/Fun.Compiler/Elaborator.Enum.cs` (`EnumCaptureLevels`),
  `Elaborator.RecTypes.cs` (`PredictCaptures`, `CompletePending`)
- the prototype's `capture_payloads` and `enclosing_scope`
