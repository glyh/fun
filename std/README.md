# `std` — the prelude

The prelude is split along the seam the compiler imposes: a **bootstrap** layer
holding exactly the names C# looks up by spelling, and a **library** layer holding
everything else. A name is in the bootstrap iff `src/` spells it; everything a
program merely uses is library.

## Layout

```text
std/bootstrap.fun   Bool, Option, List, Syntax — the ABI, and the whole of it.
                    Elaborated with no elaborator (Prelude.Load, std: null).
std/list.fun        the first cut of the library's public surface: rev, append,
                    map, fold, and Option's option_map / option_bind.
std/lib.fun         if, i64_to_bool's users, the operators with their fixity,
                    the Eq trait and its impls.
std/type.fun        the `type` macro and its token helpers.
std/stage2.fun      the `std` unit a program imports: it re-exports the library.
std/README.md       this file.
```

Each unit is elaborated once per process, against the units below it
(`src/Fun.Compiler/Prelude.cs` holds the order). The bootstrap sees only the
builtins; a library unit sees the builtins with the units below it served to its
imports; `std` is bound as `stdlib` in a program's base context. **`std` is the
only unit a program may import** — `import "std/bootstrap"` is `import not
found`, the same as any other path a loader does not serve.

A library unit re-exports the units a form it defines resolves names against:
`if` writes `True` and `False`, so `std/lib` re-exports the bootstrap, and a
consumer of `if` finds them through `std/lib`'s members. `export M` re-exports
`M`'s public macros as well as its roles and values, so a form re-exported by
`std` still reaches the macro it calls.

## The ABI, exactly

| What | Names | Where C# spells it |
| --- | --- | --- |
| `Syntax` nominals | `Expr`, `Decl`, `Pattern`, `TokenTree`, `R` | `Reflection.cs` |
| | `Explicitness`, `AtomVal`, `AtomTy`, `Fixity`, `MacroAnn` | `Reflection.cs` |
| | `TokenKind`, `Delim`, `Assoc`, `Role`, `Order`, `RoleMeaning` | `Reflection.cs` |
| | `Rule`, `RulePart`, `HoleKind`, `Replacement` | `Reflection.cs` |
| | `Capture`, `Captured`, `Field`, `QuoteHole`, `Param` | `Reflection.cs` |
| | `EffectRow`, `EffectOp`, `Ctor`, `Branch`, `PatField` | `Reflection.cs` |
| `Syntax` structures | `Id`, `Span`, `Path`, `PathChoice`, `Decls` | `Reflection.cs` |
| Three prelude nominals | `Bool`, `Option`, `List` | `Reflection.cs` |
| Constructors read by name | `Tok`, `IdentTok`, `RawVar`, `RawPatBind` | `QuoteHoles.cs` |
| Syntax C# spells | `List`, `Decl`, `TokenTree`, `Type` | `Expander.Macros.cs` |
| Unit paths and the binding | `std`, `std/bootstrap`, `stdlib` | `Prelude.cs` |

The compiler reaches the prelude's shapes through the builders
`std/bootstrap.fun` publishes (19 of them: `mk_option`, `mk_list`, `mk_span`,
`mk_id`, `mk_path`, `mk_path_choice`, `i64_to_bool`, `explicitness`, `fixity`,
`delim`, `assoc`, `hole_kind`, `atom_ty`, `macro_ann`, `pat_wild`, `pat_var`,
`pat_atom`, `pat_prod`, `pat_or`), so no constructor, field or leaf tag is
spelled in C#. `i64_to_bool` stays in the bootstrap for that reason, even though
it is otherwise a library function.

Making the table an executable declaration, instead of prose, is
[Declare the bootstrap↔compiler interface once][decl] on the design map.

[decl]: ../docs/wayfinder/tickets/declare-bootstrap-compiler-interface-once.md

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
