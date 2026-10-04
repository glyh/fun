# filter is None when the predicate fails
match (Std.Options.filter(fn(x) { x > 50 }, Some(41))) { None => True, Some(_) => False }
