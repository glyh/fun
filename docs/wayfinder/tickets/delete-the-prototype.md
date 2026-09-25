---
title: "Delete the OCaml prototype, leaving the C# port as the implementation"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: unknown residues: the rec-enum unnamed-capture throw and the synonym collector's narrower walk (read-only probe in flight, 2026-09-25)
---

# Delete the OCaml prototype, leaving the C# port as the implementation

Requested by the user 2026-09-25, with the bar stated narrowly on purpose:

> For every program the prototype **answers** (a value, or `ok`), the port answers the same.
> A prototype *error* is not an answer, so a shape both implementations refuse does not block
> deletion. Fixes that go beyond that can come later.

So this ticket is **not** "finish parity work" — it is "remove the reference implementation once
nothing is left that depends on it".

## The gate

1. **The two unknowns are settled** — the rec-enum unnamed-capture throw
   (`Elaborator.RecTypes.cs`) and the synonym collector's narrower-than-prototype walk
   (`CollectSynonymMetas`), whose failure mode is a **silent** miss rather than a refusal.
   A read-only probe is in flight; each must end *reachable, here is the program* or
   *not reachable, here is why*. **After deletion these become unfalsifiable** — there is no
   reference left to compare against — which is the one thing that cannot be fixed later.
2. **The differential harness reads `port-fails: 0`** on the final tree (`scripts/differential.sh`;
   every `.fun` in the repo through both runners).
3. Nothing else. Specifically **not** required: the capture fix, generic impls, the non-nominal
   pattern head, nested field patterns, identity-survives-re-evaluation, the synonym's implicit
   type parameters, the hang, or the seven unreachable refusal sites. Each is a fix or a feature,
   not a place where the prototype answers and the port does not.

**Known blockers at zero.** The last two — a sealed-nominal head (prototype `VALUE 1`) and a
synonym over a stuck neutral (prototype `VALUE 1`) — closed 2026-09-25
([one](port-pattern-synonym-over-sealed-nominal-head.md),
[the other](port-synonym-generalises-over-neutral.md)). The audit's other findings were the
prototype producing a *language error*, which the bar above rules out as an answer, and one ruled
change that deliberately made both implementations refuse.

## What is deleted

`lib/`, `bin/`, `test/{backend,semantic,syntax}/`, `test/conformance/run_conformance.ml`, `dune`,
`dune-project`, `fun.opam`, `_opam/`, `scripts/differential.sh`, `scripts/README.md`.

**Verified: nothing under `dotnet/` reads any of it.** No `lib/` reference in `dotnet/**/*.cs` or
`dotnet/**/*.fun`, and the port already carries its own prelude copies in `dotnet/std/`.

## What goes with it, and what it costs

- **`test/conformance/prototype-divergences.txt`** — it exists only for the OCaml runner
  ("the OCaml runner expects every case listed here to FAIL"). With the prototype gone, its
  **31 entries become ordinary cases**: each `.expect` already states the correct behaviour and the
  port already passes all of them. Fold it into a historical note — the suite loses nothing.
- **`scripts/differential.sh` and `scripts/README.md`** — comparing two implementations is their
  entire purpose.
- **`docs/STATUS.md`** — written as the *prototype's* status document; afterwards it is the port's,
  and its standing line "the OCaml prototype is not maintained once the port is done" becomes
  satisfied rather than pending.
- **`CLAUDE.md`** (the root contribution guide: dune commands, the `lib/` layout, the strict
  3000-line limit, the Alcotest rules) and anything else naming `dune` needs a pass.
- **The tickets' `lib/…` line references become dangling.** They are records, not instructions —
  leave them (rewriting 168 tickets costs more than it buys), but a reader following one finds
  nothing, so say so in the note.

## Sequencing trap worth respecting

**Land the branches that touch `prototype-divergences.txt` before deleting.** Two in-flight forks
add entries to it (the capture fix and generic impls), and their branches are cut from commits
where the file still exists. Delete it first and those merges become a conflict with a file whose
whole meaning has just been retired — resolvable, but the resolution is guesswork about which side
of a deleted convention to keep.

Hygiene, not a gate: the three in-flight branches base on pre-deletion commits and were verified
with `dune` present. Their OCaml half will not exist afterwards, which is fine for the two declared
C#-only, but say it in the merge record rather than discovering it.
