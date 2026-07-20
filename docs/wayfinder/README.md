# wayfinder

This subtree holds the project's **direction map** — the wayfinder artifact that
charts how loose ideas become decisions. A map is an *index, not a store*: it gists
each decision in one line and links to the topic doc that holds the detail, tracks
open questions as tickets, and writes down still-dim directions as fog.

- **[`fun-design-map.md`](fun-design-map.md)** — the map (labelled `wayfinder:map`).
  Start here. Its Notes / Decisions-so-far / Fog / Open-questions sections are the
  whole project direction at low resolution.
- **[`tickets/`](tickets/)** — one file per open or closed question; a ticket's
  title is its name, and its front-matter carries `status`, `parent`, and
  `blocked_by` edges.
- **[`topics/`](topics/)** — the detail behind decisions and the context behind
  open tickets (the former loose numbered plan docs).
- **[`macro-system/`](macro-system/)** — the macro design + reference library
  (plans, papers, extracted Klister notes) and its own canonical macro status.

Live implementation status (*what is built*) is **not** a wayfinder artifact and
lives outside this subtree, at [`docs/STATUS.md`](../STATUS.md).
