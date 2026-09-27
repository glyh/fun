# Content-addressed codebase database

A fog-stage direction named after its reference implementation: **Unison**
(<https://www.unison-lang.org/>, the idea in one page at
<https://www.unison-lang.org/docs/the-big-idea/>). Replace source *files* with an
immutable, hash-addressed AST database, so that expansion, elaboration and heavy
unification results are cached under the content they actually depend on and are
never invalidated.

## The reference: what Unison does

Each Unison definition is identified by **a hash of its syntax tree** —
content-addressed, using 512-bit SHA3. Named arguments are replaced by
positionally-numbered variable references and every dependency is replaced by its
hash, so a definition's hash pins down its exact implementation *and* its
dependencies. Names are separately stored metadata: a name is a pointer to an
address, and the contents of an address never change.

The consequences Unison draws from that one decision are the payoff list for any
language considering it:

- **No builds.** A definition, once parsed and typechecked, is stored with the
  result. The cache is never invalidated, and it is part of the codebase format
  rather than transient state in an editor. Deterministic (pure) test results are
  cached the same way.
- **No dependency conflicts.** Conflicts come from definitions competing for
  names; with reference-by-hash the diamond problem dissolves, and two versions of
  a library can coexist.
- **Structured refactoring and codebase tools.** Because the codebase is a
  database rather than text, renames do not break anything (they move names, not
  definitions), and the tooling can search by type, find usages and do structured
  patches. The interface is the **UCM** (Unison Codebase Manager), with libraries
  distributed through Unison Share.
- **Distributed code.** A computation can move to another location and its missing
  dependencies sync on the fly, then stay cached.

For `fun` the interesting part is narrower than the whole vision: expansion is
the expensive, macro-heavy stage, and expansion output for a given definition is
a function of content. A content-addressed store is the natural place for it.

## Where `fun` stands

There is no persistence and no content addressing. `Loader`
(`src/Fun.Compiler/Loader.cs`) resolves `import "path"` to `<cwd>/path.fun`,
reads text, and memoizes **within one run**:

- `_expanded` — path → (expanded unit, its syntax exports, its expander)
- `_loaded` — path → (value, type)

Both are keyed by path and live only for the process; every `dotnet run` and
every test run re-reads and re-expands the prelude and every imported unit.
`MacroEntry` compilation is cached inside those unit entries, so it is redone run
to run as well. `Driver` (`src/Fun.Compiler/Driver.cs`) is the only pipeline
entry point and takes source text.

## What would have to be true for `fun`

Three obstacles are specific to this language, and they are what make this fog
rather than a port of a known design.

**1. Scope sets are not stable content.** Hygiene in `fun` is sets-of-scopes:
`Syntax.Id` carries a `ScopeSet`, which is an `ImmutableSortedSet<int>`
(`src/Fun.Kernel/ScopeSet.cs`, `Syntax.cs`) of ids allocated from a counter that
varies within and between runs. Hash the AST as it stands and the *same source*
hashes differently each run. A hash must therefore be taken over
**scope-normalized** syntax — alpha-equivalence modulo scope-set renaming — and
the same canonicalization has to cover nominal ids (`Core.Refs.cs`, and the
run-time-generative identities in `Elaborator.Generative.cs`) and metavariables.
None of that canonical form exists today; it is the real work in this direction.

**2. Expansion output is not content-only.** Unison gets per-definition caching
because there are no macros and nothing ambient: a definition means what it means
by itself. In `fun`, the semantic driver elaborates one binding at a time against
the context the bindings above it built, and macro behaviour depends on that
context. So a cache key here is a **pair** — definition content plus the hash of
the elaborated context it sits in — or it is unsound: edit a macro above a
definition and the definition's expansion legitimately changes.

**3. Modules are first-class values.** A module can be built by a function and
returned, so there is no static, closed set of modules to hash. This is the same
constraint that ruled out a global impl registry in
[impl-visibility](impl-visibility.md) — what can be content-addressed is the
*definition*, not the module it may be assembled into.

There is also a bootstrap question: the prelude is text compiled in stages
(`std/stage1.fun`, `std/stage2.fun`, embedded as resources by
`src/Fun.Compiler/Fun.Compiler.csproj`). A database whose library story is
hash-addressed still needs a stage-0 source.

## What it would touch

- **Identity.** A stable structural serialization of `Syntax` / `Binding` to hash
  over, plus the scope-normalization above.
- **The loader and imports.** `import "path"` becomes an indirection: path is a
  human name for a hash, and the store is the database. `IMacroRuntime.LoadSyntax`
  is the single place expansion asks, so it is the seam.
- **Cache-key correctness** for the interleaved driver (obstacle 2).
- **Diagnostics and tooling.** Spans today point into files; a hash-addressed
  store makes "where was this written" a lookup. The whole edit/compile loop moves
  from `dotnet build` to a store.
- **The store itself** — on-disk format, garbage collection, hash-mismatch
  reporting, and a UCM-shaped interface over it.

## Why this is fog, not a ticket

Nothing here changes what programs mean, and the payoff requires a persistent
store that does not exist. The tree is small and a full compile is fast, so the
cost being optimized is **unmeasured**. Building a database before measuring
compilation is optimizing a cost nobody has paid.

It also sits behind decisions that are already open and come first:
[restructure `std` into a bootstrap layer and a library layer](../tickets/restructure-std-into-bootstrap-and-library.md)
and [declare the bootstrap↔compiler interface once](../tickets/declare-bootstrap-compiler-interface-once.md)
settle the *shape* of the codebase boundary. A content-addressed store makes that
boundary hash-shaped rather than path-shaped, so the interface should land first
and inform the store rather than the reverse.

**Sharpens when** compile time is measured and attributed — the frontier already
lists [the runner does not timebox elaboration](../tickets/port-runner-does-not-timebox-elaboration.md)
and [deep non-tail recursion](../tickets/deep-non-tail-recursion-is-superlinear.md)
as the known performance items — or when a hash-addressed library distribution
story is actually wanted. The ticket then starts from a measured cost, names the
stage being cached (expansion, elaboration, unification), and begins with the
scope-normal form, which is the piece that is missing regardless of the store.
