# range(n) is the 0-based indices below n
match (Std.Lists.range(3)) { Cons(_, Cons(x, _)) => x, _ => 0 }
