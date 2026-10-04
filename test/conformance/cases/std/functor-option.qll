# one trait, a second container: the same use at Option
Std.Functors.Functor.map(fn(x) { x + 1 }, Some(1)) == Some(2)
