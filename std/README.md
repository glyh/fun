# `std` — the prelude

Stage 1 (`stage1.fun`) is the static half: the builtins' companions, the `Syntax`
reflection module and the builders the compiler reaches through. Stage 2
(`stage2.fun`) imports stage 1 as its own unit `stdlib` and re-exports it. The base
context binds stage 2 as `stdlib` (glossary: Base context, Prelude).

## The interface is declared on the compiler side

`src/Fun.Kernel/PreludeAbi.cs` **is** the bootstrap↔compiler interface: the builders
the prelude publishes, the type and module names the compiler names, and the
constructor tags it writes and reads back. The compiler spells a prelude name **only
there**; every consumer references the declaration. `Prelude.Verify` resolves the
whole declaration when stage 1 loads, so a rename here is a load error naming the
member and the file it was looked for in, not a silent drift discovered at first use.

The declaration mirrors the prelude's shape, and that is the accepted cost of the
chosen route: the interface is a fact about the compiler's *usage* — which names
matter, in which role — which the prelude source alone cannot supply. Generating the
C# names from `stage1.fun` was deferred for that reason, and because a
`netstandard2.0` Roslyn generator cannot reference `Fun.Expand`; it would have to be
an MSBuild `Exec` of a console tool.

Two things are deliberately not in the declaration:

- **`Type`** — the compiler spells it, and it belongs to the elaborator
  (`src/Fun.Compiler/Elaborator.cs`), not to `std`.
- **The unit paths** — `Prelude.Path`, `Prelude.Stage1Path` and `Prelude.Binding` are
  their single source; the declaration references them, it does not restate them.
