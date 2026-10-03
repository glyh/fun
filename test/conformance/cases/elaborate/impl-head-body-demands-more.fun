# from docs/wayfinder/tickets/impl-head-written-bound.md: the body may use at most what
# the head writes - a body demanding Show(a) while the head writes only `[a : Eq]` is an
# error at the definition, not a wider declaration
{ trait Eq(a) = sig { eq : a -> a -> Bool }; trait Show(a) = sig { show : a -> I64 }; impl probe[a : Eq] : Eq(List(a)) = module { fn eq(xs, ys) { match (xs) { Nil => True, Cons(h, t) => match (Show.show(h)) { _ => True } } } }; 0 }
