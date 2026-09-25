---
title: "Delete the OCaml prototype, leaving the C# port as the implementation"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 - the user approved once both halves of the gate were met (port-fails 0 measured; both unknowns settled as non-blockers, one of them by correcting the ticket's own premise). 99 tracked files deleted, the divergence file folded into a historical note, and the port re-verified after the deletion. One real breakage the deletion introduced was found and fixed: the conformance runner located the cases by walking up for `dune-project`.
assignee:
blocked_by:
---

# Delete the OCaml prototype, leaving the C# port as the implementation

> ## Resolution (2026-09-25) — done
>
> Approved by the user once **both halves of the gate were met**: the differential harness read
> `port-fails: 0` over 766 programs with all 34 disagreements being recorded divergences, and the
> two unknowns were settled as non-blockers —
> [the rec-enum unnamed-capture throw](port-rec-enum-over-captures-names.md) after 16 probes across
> the plausible capture routes plus a monotone-fixpoint argument, and the collector residual by
> **disproving this ticket's own premise** (the prototype's `PatternSynBinding` path,
> `elab_infer.ml:503`, does no generalization at all, so "the port collects fewer metas than the
> prototype" was impossible).
>
> **Deleted** (99 tracked files): `lib/`, `bin/`, `test/{backend,semantic,syntax}/`,
> `test/conformance/{dune,run_conformance.ml}`, `dune`, `dune-project`, `fun.opam`, `.envrc`, and
> `scripts/` (the differential harness and its README). `_opam`/`_build` removed untracked.
> **Kept**: `dotnet/`, the 766 shared programs in `test/conformance/cases/`, `docs/`, `CONTEXT.md`.
>
> **One breakage the deletion introduced, found before committing**: the conformance runner found
> the cases by walking up for `dune-project` — the very file being deleted. `CasesRoot` now walks
> up for `test/conformance/cases` itself, and its error message and two comments that named the
> deleted harness are corrected. Verified afterwards: build clean, `185/185` xUnit,
> `766 cases, 0 failed`, and the `--file` probe protocol still answers.
>
> **The divergence file is folded, not removed.** `test/conformance/prototype-divergences.txt` keeps
> its 34 lines under a header saying the second implementation is gone and that nothing reads the
> file: each listed case is an ordinary one now, and `grep` for readers returns none. The list is
> kept because 34 programs where a second implementation got the language wrong are worth having
> beside the cases.
>
> **Docs rewritten to match**: `README.md` (implementation, commands, and the removal with its
> reason), `CLAUDE.md` (the port's build/test/layout, the live conventions promoted, and the
> prototype's sections kept as *History* with the knowledge that transfers), `docs/STATUS.md`
> (header now the port's, with the prototype's entries marked as its record), `cases/README.md`,
> `.gitignore` (dotnet rather than OCaml artefacts), and the design map's worktree note.
>
> **What this does not claim**: that the port is correct, only that it is a superset — the 34-fold
> divergence list and today's three audited gaps were all found by *probing*, and the harness's own
> failure to see any of them is the reason the gate was phrased as a measurement rather than a
green suite.

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
