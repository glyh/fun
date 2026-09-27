# `std` — the prelude

The prelude is split along the seam the compiler imposes: a **bootstrap** layer
holding exactly the names C# looks up by spelling, and a **library** layer holding
everything else. A name is in the bootstrap iff `src/` spells it; everything a
program merely uses is library.

## Layout

```text
std/bootstrap.fun   Bool, Option, List, Syntax — the ABI, and the whole of it.
                    Elaborated with no elaborator (Prelude.Load, std: null).
std/lib.fun         if, the operators with their fixity, the bare one-per-language
                    helpers (not, and, or, min, max, abs), the Eq trait and impls.
std/list.fun        Lists: the list library — one unit per module, every binding pub.
std/option.fun      Options: the option library.
std/type.fun        the `type` macro and its token helpers.
std/stage2.fun      the `std` unit a program imports: it publishes the modules as
                    members (Std.Lists, Std.Options) instead of flattening them.
std/README.md       this file.
```

Each unit is elaborated once per process, against the units below it
(`src/Fun.Compiler/Prelude.cs` holds the order). The bootstrap sees only the
builtins; a library unit sees the builtins with the units below it served to its
imports; `std` is bound as `Std` in a program's base context. **`std` is the
only unit a program may import** — `import "std/bootstrap"` is `import not
found`, the same as any other path a loader does not serve.

`std/list` and `std/option` sit *above* `std/lib`, so a list or option binding is
written in the language's own surface (the operators, `Bool`) rather than around it.

A library unit re-exports the units a form it defines resolves names against:
`if` writes `True` and `False`, so `std/lib` re-exports the bootstrap, and a
consumer of `if` finds them through `std/lib`'s members. `export M` re-exports
`M`'s public macros as well as its roles and values, so a form re-exported by
`std` still reaches the macro it calls. `export` *flattens* and `open` only
*scopes*, which is why `std` flattens the language surface (`export Types`) and
publishes `Lists`/`Options` as members: a bare `map` would otherwise be resolved
by open order rather than by intent.

## The library's public surface

After `import "std"` — or the entry point's implicit `open (import "std")` — a
program reaches the library by qualification:
`Std.Lists.map(f, xs)`, `Std.Options.get_or(d, o)`. A bare `map` or `length`
exists only after the program's own `open Std.Lists`, so there is exactly one
spelling of each name and two modules cannot shadow each other silently. The
bare one-per-language names stay unqualified.

| Module | Bindings |
| --- | --- |
| `Std.Lists` | length, reverse, append, concat, map, filter, fold, find, head, tail, nth, head_or, nth_or, take, drop, zip, zip_with, any, all, range |
| `Std.Options` | map, bind, get_or, or_else, filter, is_some |
| `Std` top level | if, `==` `!=` `<` `>` `<=` `>=` `+` `-` `*` `/` `%`, not, and, or, min, max, abs, `Eq` and its five primitive impls |

The module names are plural because the singulars (`List`, `Option`, `Bool`,
`String`) are the ABI's type names and cannot be reused: `List.map` is "no
constructor `map`". **`Strings` is named but not declared** — nothing a `Strings`
module could hold is writable above the primitive floor (`String.length`,
`concat`, `split`, `chars` each need a primitive that does not exist), while
`==`/`!=` on strings already work through the string `Eq` impl. An empty public
module would be a name with nothing behind it. There is no `Show` and no `Ord`.

Every operation is total, so no accessor panics: `head`/`tail`/`nth`/`find`
answer an `Option` (with a default first: `head_or(d, xs)`, `nth_or(d, i, xs)`),
`take`/`drop` saturate, `zip`/`zip_with` truncate to the shorter list, indices are
0-based, and `range(n)` is `[0, n)` — `Nil` for `n <= 0`. Policy comes first and
the subject last (`map(f, xs)`, `fold(f, z, xs)`, `nth(i, xs)`), which is what
makes `take(2)` and `nth(0)` useful as partial applications. Each `pub` binding
carries one comment line saying what it returns and what it does on the
empty/out-of-range case; a module's first line says what belongs in it.

**`std` is the only unit a program may import.** Everything above arrives through
it — there is no `import "std/list"`.

## The ABI, exactly

| What | Names | Where C# spells it |
| --- | --- | --- |
| `Syntax` nominals | `Expr`, `Decl`, `Pattern`, `TokenTree`, `R` | `PreludeAbi.Types` |
| | `Explicitness`, `AtomVal`, `AtomTy`, `Fixity`, `MacroAnn` | `PreludeAbi.Types` |
| | `TokenKind`, `Delim`, `Assoc`, `Role`, `Order`, `RoleMeaning` | `PreludeAbi.Types` |
| | `Rule`, `RulePart`, `HoleKind`, `Replacement` | `PreludeAbi.Types` |
| | `Capture`, `Captured`, `Field`, `QuoteHole`, `Param` | `PreludeAbi.Types` |
| | `EffectRow`, `EffectOp`, `Ctor`, `Branch`, `PatField` | `PreludeAbi.Types` |
| `Syntax` structures | `Id`, `Span`, `Path`, `PathChoice`, `Decls` | `PreludeAbi.Types` |
| Three prelude nominals | `Bool`, `Option`, `List` | `PreludeAbi.Types` |
| Constructors read by name | `Tok`, `IdentTok`, `RawVar`, `RawPatBind` | `QuoteHoles.cs` |
| Syntax C# spells | `List`, `Decl`, `TokenTree`, `Type` | `PreludeAbi` / `Elaborator` |
| Unit paths and the binding | `std`, `std/bootstrap`, `Std` | `Prelude.cs` |

The compiler reaches the prelude's shapes through the builders
`std/bootstrap.fun` publishes (19 of them: `mk_option`, `mk_list`, `mk_span`,
`mk_id`, `mk_path`, `mk_path_choice`, `i64_to_bool`, `explicitness`, `fixity`,
`delim`, `assoc`, `hole_kind`, `atom_ty`, `macro_ann`, `pat_wild`, `pat_var`,
`pat_atom`, `pat_prod`, `pat_or`), so no constructor, field or leaf tag is
spelled in C#. `i64_to_bool` stays in the bootstrap for that reason, even though
it is otherwise a library function.

## The interface is declared on the compiler side

`src/Fun.Kernel/PreludeAbi.cs` **is** the bootstrap↔compiler interface: the builders
the prelude publishes, the type and module names the compiler names, and the
constructor tags it writes and reads back. The compiler spells a prelude name **only
there**; every consumer references the declaration. `Prelude.Verify` resolves the
whole declaration when the bootstrap loads, so a rename here is a load error naming
the member and the file it was looked for in, not a silent drift discovered at first
use.

The declaration mirrors the prelude's shape, and that is the accepted cost of the
chosen route: the interface is a fact about the compiler's *usage* — which names
matter, in which role — which the prelude source alone cannot supply. Generating the
C# names from `bootstrap.fun` was deferred for that reason, and because a
`netstandard2.0` Roslyn generator cannot reference `Fun.Expand`; it would have to be
an MSBuild `Exec` of a console tool.

Two things are deliberately not in the declaration:

- **`Type`** — the compiler spells it, and it belongs to the elaborator
  (`src/Fun.Compiler/Elaborator.cs`), not to `std`.
- **The unit paths** — `Prelude.Path`, `Prelude.BootstrapPath` and `Prelude.Binding`
  are their single source; the declaration references them, it does not restate them.

## Why the `Syntax` ADT is one `rec … and …` chain

`Expr` holds `List(Branch)`, `Branch` holds `Pattern`, and `Pattern` holds
`Expr`, so the types are genuinely mutually recursive; splitting them into
separate `rec` declarations would break the reflection round trip. Read it as one
declaration, whatever the line breaks suggest.

## One wrinkle of provenance

This source began as the deleted prototype's `stage1_source` and `stage2_source`
OCaml string literals (see
`git show 37b41f1^:lib/semantic/typecheck/elab_prelude.ml`). That is why the ADT
arrived as one enormous line, and why the comments read the way they do — it is
not a deliberate style. The layout was normalised on 2026-09-27.
