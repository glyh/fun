---
title: Refresh the domain-model docs' "today" sections
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: The "today" and "Model decision vs today" sections of all four domain-model topics were checked against main and rewritten; history is marked as history and every remaining distance names its open ticket. The struct-open ticket's stale line references are left as-is (that ticket is closed).
assignee:
blocked_by:
---

# Refresh the domain-model docs' "today" sections

Found by the domain-model audit (2026-09-15).

## Why

The domain-model topics describe "today" as it was before the 2026-09-14/15 runs.
The model sections still hold; the descriptive "today" prose is wrong against the
code.

## Stale sections

- `topics/core-tt-domain-model.md`:
  - I4c: says roles are keyed by string, newest wins. They now resolve by scope
    set, with `RoleConflict`.
  - I4d: table and "Why" describe the pre-change behaviour with no history marker.
  - I4e: body describes the latch as live.
  - I5: says the import "returns the term, discarding the value". The reverse is
    true.
  - I1: says nothing checks widths early. `fold_left2` in `extend_from_slots` now
    raises on a mismatch.
- `topics/core-tt-domain-model-macros.md`:
  - "Model decision vs today" (line 252): 7 of 12 rows are wrong.
  - "Today" paragraphs in M1 (line 45), M3, M5, M7, M8, M9, M11.
  - M12 claims resolved names are always fresh (see
    `declaration-binders-keep-written-names`).
- `topics/core-tt-domain-model-surface.md`:
  - "Three macro paths, three hygiene contracts (today)" (line 117): there is one
    contract.
  - "The seam (today)" (line 150): `Surface.t` is deleted.
  - S2, S4, S7 describe fixed behaviour.
- `topics/core-tt-domain-model-effects.md` "Model decision vs today" (line 148):
  not audited; the effects audit did not finish.
- `tickets/struct-open-does-not-scope-over-con-fields.md`: line references moved
  (`enforest.ml:1730` → ~1462, `expand.ml:419` → ~661) and it cites the deleted
  `Surface.OpenBinding`.

## Direction

Replace each "today" description with a one-line pointer: to the closed ticket
that removed the distance, or to the open ticket that is the remaining distance.
