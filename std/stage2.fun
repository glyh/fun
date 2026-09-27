# The `std` unit a program imports: it re-exports the library units. Nothing below
# `std` is importable by a program, so only this unit is a program's face of the
# prelude. `std/type` re-exports `std/lib`, which re-exports the bootstrap, so the
# whole ABI and language surface arrives through it. The list and option modules
# arrive as members (`Std.Lists`, `Std.Options`) rather than flattened, so a program
# reaches them qualified or through its own `open`.
Types = import "std/type";
export Types;
open Types;
pub Lists = import "std/list";
pub Options = import "std/option";
