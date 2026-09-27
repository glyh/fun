# find is None when the predicate holds for no element
match (Std.Lists.find(fn(x) { x > 9 }, Cons(1, Cons(2, Nil)))) { None => True, Some(_) => False }
