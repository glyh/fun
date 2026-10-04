---
title: Load std units in any order, pinning only the bootstrap
parent: ../quill-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Load std units in any order, pinning only the bootstrap

## The complaint, measured

Adding `std/functor` required editing `Prelude.Order` (`src/Quill.Compiler/Prelude.cs:29`), a
fixed list, lowest first. Without that edit the import fails with

```
ELAB import not found: "std/functor"
```

**even though the source is embedded and the build is clean** — `strings` on
`Quill.Compiler.dll` lists `std/functor.qll` alongside `std/bootstrap.qll` and the rest. The
resource is there; the loader simply never offers the path, because `Prelude.Of` answers
only for paths that array names.

So the standard library carries a **hand-maintained topological order**. It is seven
entries today, every new unit is another insert whose position has to be right, and the
failure when it is not names nothing about the array — the message says the file is
missing, and it is not.

## What the order is actually for

Four things read it:

- **`Index` / `Of`** (`Prelude.cs:32,42`) — path → stage. A path the array does not name
  throws `not a prelude unit`.
- **`Load(path, Order.Take(i))`** (`Prelude.cs:39,70`) — each unit is elaborated with a
  `Loader` whose `_prelude` is the units **below** it, so its own imports resolve only
  downwards.
- **`Loader.Stdlib`** = `_prelude[^1]` (`Loader.cs:27`) — the unit bound as `Std` while
  that unit elaborates. The order therefore decides what `Std` means *inside* a std unit:
  for `std/functor` it was `std/option`, which is arbitrary.
- **Metas seeding** — `metas.SeedFrom(Prelude.Of(prelude[^1]).Metas)` (`Loader.cs:73`),
  which is what makes an exported macro's value speak of the same metas wherever it is
  used.

One unit is genuinely special, and the ticket keeps it pinned: the **bootstrap**, whose
stage is `SyntaxStage` — what reflection reads `Syntax` off, "so reading it does not wait
on a unit whose own body quotes while it is still being elaborated" (`Prelude.cs:53`) —
and whose `Load` runs the whole `PreludeAbi` check.

## Wanted

Pin the bootstrap. Let every other unit be found by asking for it.

1. **Demand-load.** `import "std/X"` resolves by loading `X`, recursively, instead of by
   `X` having to sit below its importer in a list. Adding a unit becomes adding a file.
2. **Derive the order.** Keep `Order`/`below` exactly as they are, but compute the list by
   walking imports from `stage2` and sorting topologically, instead of writing it by hand.
   The smallest change that removes the hand-maintenance, and it preserves every semantic
   the four readers above depend on.

(2) is the cheaper of the two and may be all that is wanted; (1) is the one that also
removes the *position* question rather than only the editing.

## Open questions

1. **What replaces `below`?** Demand-loading makes it "the units this one imports,
   transitively" — but `Loader` takes a *list* and `Stdlib` is its **last** element. In a
   DAG, "last" is undefined: a unit may import two, neither after the other.
2. **What is `Std` inside a std unit?** Today it is whatever the array happened to put
   directly below. Nothing in `std/` appears to use it, and `stage2.qll` says only the
   topmost unit is a program's face — so the ticket should rule whether it becomes the
   bootstrap, or disappears for std units. A ruling, not a discovery.
3. **Cycles.** The current order cannot express one, and `Expanded` refuses them loudly
   (`circular import`, `Loader.cs:41`). Demand-loading needs a ruling: keep refusing, or
   knot them the way mutually-recursive nominals are knotted.
4. **Metas seeding under a DAG.** `SeedFrom(prelude[^1].Metas)` assumes a topmost unit.
   Which unit's metas seed which, when the graph branches?
5. **The program's import rule.** "Nothing below `std` is importable by a program"
   (`stage2.qll`) is a separate visibility rule and must not fall out of this change by
   accident.

## Sharpens when

The next std unit is added. That has now happened once — `std/functor`, `7ce4aed` — and
cost a compiler edit plus two rebuilds to discover why a correctly embedded file was
"not found".
