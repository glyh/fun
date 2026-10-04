# take keeps the first n elements
match (Std.Lists.take(2, Cons(1, Cons(2, Cons(3, Nil))))) { Cons(_, Cons(x, _)) => x, _ => 0 }
