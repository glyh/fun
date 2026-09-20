---
title: "Port: audit every unported path against the prototype"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: audit every unported path

Step 3 of [port-parity-plan](port-parity-plan.md). **This is an audit — it writes no
feature code.** Its deliverable is this file: one verdict per site, so the invisible
delta becomes a number and the real gaps become tickets.

`dotnet/test/Fun.Conformance/Program.cs` counts a `NotImplementedException` as a
failure *only when a case reaches it*. So a `not ported yet` path that no shared case
exercises is a silent hole in "the port is complete". There are **60
`NotImplementedException` sites in 25 files** under `dotnet/src` (58 of them spelled
"not ported yet") and **15 `ponytail:` stopgaps** — the stopgaps already state their
own reason, so they need only a verdict, not a diagnosis.

## Method

For each site: read the throw and the switch/list around it, name the program form
that reaches it, then decide which of three it is.

1. **Real gap** — the prototype handles that form. Write the smallest program in a
   scratch file (not in `test/conformance/cases`), run it in both runners, and
   report the prototype's outcome. The real gap is a ticket line; the case is added
   by whoever ports it (the conformance count must never drop).
2. **Parity** — the prototype also refuses (or has no such form). Then the throw is
   wrong about the *language*: it must become the proper `FunException` so a case
   expecting `error` can pass for the right reason (porting convention 2). Cite the
   prototype file and line that refuses it.
3. **Unreachable** — no program reaches it; it is a lying catch-all. Say why.

Use the prototype as a *map*, not as the spec — where they disagree, the domain
model decides and a prototype defect is its own verdict (convention 5). Two
verdicts are already established and are examples of 2:
`Enforest.cs:412` bare bracket expressions (`lib/expand/enforest.ml:87`, "not in
Phase 7A") and `Enforest.Roles.cs:338` a dotted order-group reference (an open item
on [brackets-decide-grouping](brackets-decide-grouping.md)).

## The sites

Grouped by file. Add `verdict` + `prototype reference` to each; add rows if reading
the code turns up a path not listed here (the grep only finds the ones spelled
"not ported yet").

**`Fun.Kernel/`** — traversals (a missing case here means some *form* cannot be
walked at all, which is a real gap for every feature that can contain it)
1. `Core.Shift.cs:67` traversing `{term}`
2. `Core.Shift.cs:107` traversing the binding `{binding}`
3. `Syntax.Map.cs:174` a syntax traversal over `{form}`
4. `Syntax.Map.cs:225` a syntax traversal over the binding `{binding}`
5. `Syntax.Map.cs:256` a syntax traversal over the pattern `{pattern}`

**`Fun.Expand/`** — the reader/enforester and expander
6. `Enforest.cs:221` module item starting `` `{head}` ``
7. `Enforest.cs:291` the `` `{w}` `` form (a keyword form in expression position)
8. `Enforest.cs:387` an implicit argument written `f{ e }`
9. `Enforest.cs:396` the infix operator `` `{sym}` ``
10. `Enforest.cs:412` bracket expressions *(verdict 2, established)*
11. `Enforest.Roles.cs:148` the polymorphic arrow `~>`
12. `Enforest.Roles.cs:338` an order group named through a unit member's path *(verdict 2, established)*
13. `Enforest.Roles.cs:845` reading the statement `{statement}` as quoted syntax
14. `Enforest.Traits.cs:143` an unnamed impl in a signature
15. `Expander.cs:212` expanding `{stx}`
16. `Expander.cs:357` expanding the binding `{other}`
17. `Expander.Macros.cs:306` a type-aware operator macro

**`Fun.Compiler/Elaborator.*`** — the elaborator
18. `Elaborator.cs:304` elaborating `{stx}` (a `Syntax.t` form with no case)
19. `Elaborator.cs:442` elaborating the binding `{binding}`
20. `Elaborator.cs:506` opening a value of unknown type
21. `Elaborator.Enum.cs:40` a constructor pattern head that is a member of a non-nominal
22. `Elaborator.Enum.cs:295` the names a `{s}` uses
23. `Elaborator.Enum.cs:314` the names a `{b}` binding uses
24. `Elaborator.Enum.cs:352` the variables a `{other}` mentions
25. `Elaborator.Export.cs:52` exporting a value of unknown type
26. `Elaborator.Generalise.cs:44` deciding whether generalizing is closed
27. `Elaborator.Generative.cs:52` sealing a generative nominal not bound as a module member
28. `Elaborator.Implicits.cs:50` applying a value of unknown function type
29. `Elaborator.Imports.cs:16` an import with no loader
30. `Elaborator.Macros.cs:25` a type-aware macro's call with no expander
31. `Elaborator.Macros.cs:26` a type-aware macro's call with no loader
32. `Elaborator.Patterns.cs:47` a record pattern whose head has an unknown type
33. `Elaborator.Patterns.cs:72` a pattern synonym whose pattern does not fix its scrutinee's type
34. `Elaborator.Patterns.cs:76` a pattern synonym over a type-case pattern
35. `Elaborator.Patterns.cs:88` a pattern synonym whose parameter types are not fixed
36. `Elaborator.Patterns.cs:200` type-case on a generative nominal — **being fixed by [port-nominal-identity](port-nominal-identity.md)**
37. `Elaborator.RecTypes.cs:63` a recursive enum whose payloads capture a variable its body does not name
38. `Elaborator.Structs.cs:80` the struct item `{item}`
39. `Elaborator.Structs.cs:209` record construction through an explicit type former or a type of unknown shape
40. `Elaborator.Structs.cs:244` the signature item `{binding}`
41. `Elaborator.Structs.cs:290` method calls on a record — **step 2 (4 cases)**
42. `Elaborator.Implicits.cs`/`Elaborator.cs` others found while reading

**`Fun.Compiler/`** — the machine, unification, reflection
43. `Nbe.cs:197` evaluating `{term}`
44. `Nbe.cs:561` reading back `{other}`
45. `Nbe.Effects.cs:106` the checker evaluated a term that performs `{instance}.{op}` — **step 2 (1 case)**
46. `Nbe.Match.cs:87` a match stuck on an unknown value
47. `Nbe.StuckMatch.cs:50` the binders of a stuck `{p}` arm
48. `Nbe.StuckMatch.cs:54` reading back an unreachable arm of a stuck match
49. `Reflection.cs:177` reflecting `{form}` as a path
50. `Reflection.cs:282` reflecting the form `{stx}`
51. `Reflection.cs:372` reflecting the pattern `{p}`
52. `Reflection.cs:399` reflecting the declaration `{b}`
53. `Reflection.cs:603` reading a reflected unit token
54. `Reflection.cs:718` / `:922` a trait with other than one parameter
55. `Reflection.cs:726` / `:928` an impl of other than one argument
56. `Reflection.cs:762` reading the reflected form `{name}`
57. `Reflection.cs:850` reading reflected trait bound paths
58. `Unify.cs:199` renaming a `{n.Head}` head
59. `Unify.cs:204` renaming a `{frame}` frame — **step 2 (`elab-059`)**
60. `Unify.cs:206` solving to `{other}`
61. `Unify.Neutrals.cs:35` unifying stuck `{a.Frames[i]}` frames

Note: several of 43–61 are "the other side of a form" — `Reflection` and `Unify` are
where a *new* syntax or term kind must be taught to the refolder and the unifier.
Tally them per form, because one missing form shows up in three or four of these
lists at once and the audit should say so rather than counting them as four gaps.

## The `ponytail:` stopgaps (15)

Each already names its own reason. Verdict: **feature the prototype has** (port it),
**shared choice** (the prototype does the same — leave, and say so), or
**divergence** (the port and the prototype differ; which is right?).

- `Elaborator.cs:368`, `Elaborator.Enum.cs:48`, `Elaborator.Generative.cs:12` — no
  run-time stamp → [port-nominal-identity](port-nominal-identity.md)
- `Elaborator.Structs.cs:31` — method types read where the first method is elaborated
- `Elaborator.Effects.cs:288`, `:304` — the row read with a rigid argument; a
  codomain depending on a performing argument reads the stand-in
- `Elaborator.PolyArrows.cs:60` — variables under a higher-order parameter bound at the root (rank 1)
- `Elaborator.Macros.cs:21` — a typed macro argument elaborates twice
- `Elaborator.Refs.cs:122` — one pass over the older metas per discharge site
- `Nbe.Patterns.cs:88` — a nominal-head match nests one evaluation (no `Kont` frame)
- `Nbe.Rec.cs:99` — read-back equality has no eta and does not unfold
- `Nbe.StuckMatch.cs:12` — only an unknown *scrutinee* waits
- `Core.Rec.cs:34` — environment identity is by reference
- `Budget.cs:11` — the limit is a constant
- `ScopeSet.cs:10` — `ImmutableSortedSet` where the OCaml is a sorted list
- `Fun.Conformance/Program.cs:53` — a timed-out run's thread keeps spinning

**Also part of this step: internals parity.** The conformance suite cannot see syntax
shapes, the unifier, the machine or budget accounting, so C# coverage for those rests
on `dotnet/test/Fun.Tests` mirroring the OCaml Alcotest suites
(`test/syntax/test_*.ml`, `test/semantic/test_elaborate.ml`,
`test/backend/test_core.ml`). List which OCaml suites have no C# counterpart and
which of their cases are neither conformance cases nor xUnit tests — that is the
third invisible delta (there is no `BudgetTests.cs`, for instance).

## Report

Do not edit this ticket's siblings, `fun-design-map.md` or `docs/STATUS.md`. Report:
the verdict counts (real gaps / parity / unreachable), the real gaps in priority
order with the program that reaches each, the OCaml suites with no C# counterpart,
and every question you stopped on with a concrete example.
