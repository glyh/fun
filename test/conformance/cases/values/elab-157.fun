{ Option2 = fn(A : Type) { enum { Some2(A), None2 } }; open Option2; match (Some2(7)) { Some2(n) => n, None2 => 0 } }
