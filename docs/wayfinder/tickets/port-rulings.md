---
title: "Port: implement the verification rulings"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
