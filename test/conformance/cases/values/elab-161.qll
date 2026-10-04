{ rec L = fn(A : Type) { enum { Nil2, Cons2(A, L(A)) } }; match (L.Cons2(4, L.Cons2(2, L.Nil2))) { L.Cons2(h, _) => h, L.Nil2 => 0 } }
