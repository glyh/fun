# all is False when one element fails the predicate
Std.Lists.all(fn(x) { x > 1 }, Cons(1, Cons(3, Nil)))
