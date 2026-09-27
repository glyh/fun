# filter keeps the value its predicate holds for
Std.Options.get_or(0, Std.Options.filter(fn(x) { x > 40 }, Some(41)))
