# zip's elements are pairs taken in order
match (Std.Lists.zip(Cons(1, Nil), Cons(True, Nil))) { Cons(p, _) => p.0, _ => 0 }
