# The `std` unit a program imports: it re-exports the library units. Nothing below
# `std` is importable by a program, so only this unit is a program's face of the
# prelude. `std/type` re-exports `std/lib`, which re-exports the bootstrap, so the
# whole ABI and language surface arrives through it; the lists arrive beside it.
Types = import "std/type";
export Types;
open Types;
Lists = import "std/list";
export Lists;
open Lists;
