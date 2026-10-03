# from docs/wayfinder/tickets/impl-head-written-bound.md: an impl that writes nothing
# keeps the inferred behavior - a body that never compares needs no dictionary, so
# Foo's lack of Eq is irrelevant
{ trait Eq(a) = sig { eq : a -> a -> Bool }; rec Foo = enum { F }; open Foo; impl probe : Eq(List(a)) = module { fn eq(xs, ys) { True } }; Eq.eq(Cons[Foo](F, Nil[Foo]), Cons[Foo](F, Nil[Foo])) }
