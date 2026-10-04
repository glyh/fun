# the functor is inferred from the argument: no written [List]
Std.Functors.Functor.map(fn(x) { x + 1 }, Cons(1, Cons(2, Nil))) == Cons(2, Cons(3, Nil))
