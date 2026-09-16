# a recursive enum declared under a binder captures what its body names, and its payloads name it
{ f = fn(B : Type, b : B) { rec L = enum { Nil, Cons(B, L) }; open L; match (Cons(b, Cons(b, Nil))) { Cons(_, Cons(x, _)) => x, _ => b } }; f(I64, 7) }
