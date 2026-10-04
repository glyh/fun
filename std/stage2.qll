# The `std` unit a program imports: it re-exports the library units. Nothing below
# `std` is importable by a program, so only this unit is a program's face of the
# prelude. `std/type` re-exports `std/lib`, which re-exports the bootstrap, so the
# whole ABI and language surface arrives through it. The list, option and functor
# modules arrive as members (`Std.Lists`, `Std.Options`, `Std.Functors`) rather than
# flattened, so a program reaches them qualified or through its own `open`.
Types = import "std/type";
export Types;
open Types;
pub Lists = import "std/list";
pub Options = import "std/option";
pub Functors = import "std/functor";

# Only the impls reach a program's base scope; the modules themselves stay behind
# `Std.Lists`/`Std.Options`/`Std.Functors`, so nothing flattens. The functor impls join
# the equality ones, which is what lets a `Functor.map` use infer its constructor from
# the argument rather than needing it written.
export Lists.{list_eq};
export Options.{option_eq};
export Functors.{list_functor, option_functor};
