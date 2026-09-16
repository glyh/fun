---
title: "Port: implement the verification rulings"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: implement the verification rulings

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

Three user rulings on prototype behaviour, each with its own ticket. For each: the
port behaves as ruled, and a shared conformance case pins the ruled behaviour.

1. [check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md)
   — **needs code**: checking against an implicit function type binds its implicit
   parameter first, so `g : [A : Type] -> A -> A = id` checks.
2. [panic-with-unknown-message-fails-checking](panic-with-unknown-message-fails-checking.md)
   — the port already leaves it stuck: add the case.
3. [deref-of-unknown-type-is-not-a-reference](deref-of-unknown-type-is-not-a-reference.md)
   — the port already infers `Ref(?h, ?A)`: add the case.

Run each new case through the prototype and report which fail there; the
integrator lists those in `prototype-divergences.txt`.

## Resolution (2026-09-16)

Merged from `port/rulings` (`9104164`). A value that is not an implicit lambda,
checked against an implicit function type, is checked with that type's implicit
parameter bound first (a trait dictionary parameter is bound as evidence), so
`g : [A : Type] -> A -> A = id` checks. Shared cases, each failing in the prototype
and listed in `prototype-divergences.txt`: `values/check-against-implicit-type` (5;
prototype `CannotUnify(function type vs function type)`),
`values/panic-unknown-message-in-type` (1; prototype `EvaluationFailed "panic while
reading the type"`), `values/deref-infers-reference` (7; prototype
`ApplyingNonFunction`). C# 250/672; xUnit 123.

**Consequence of ruling 1:** an explicit lambda checked against an implicit type
(`fn(a) { a }` against `[A : Type] -> A -> A`) now checks too, where it was
"applying non-function"; the same rule. No case covers it.
