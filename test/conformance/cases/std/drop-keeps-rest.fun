# drop removes the first n elements
match (Std.Lists.drop(1, Cons(1, Cons(2, Nil)))) { Cons(x, _) => x, Nil => 0 }
