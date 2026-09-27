# Option's map is None for None
match (Std.Options.map(fn(x) { x + 1 }, None[I64])) { None => True, Some(_) => False }
