# from docs/wayfinder/tickets/impl-head-written-bound.md: a written bound narrows the
# impl deliberately - the body never compares, yet `[a : Eq]` demands Eq(a) at the use,
# so Foo's lack of Eq is refused again
{ trait Eq(a) = sig { eq : a -> a -> Bool }; rec Foo = enum { F }; open Foo; impl probe[a : Eq] : Eq(List(a)) = module { fn eq(xs, ys) { True } }; Eq.eq(Cons[Foo](F, Nil[Foo]), Cons[Foo](F, Nil[Foo])) }
