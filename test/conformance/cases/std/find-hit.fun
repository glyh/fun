# find answers the first element its predicate holds for
match (Std.Lists.find(fn(x) { x > 1 }, Cons(1, Cons(2, Cons(3, Nil))))) { Some(x) => x, None => 0 }
