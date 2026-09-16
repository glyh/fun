{ Option2 = fn(A : Type) { enum { Some2(A), None2 } }; f = fn(a : Option2(I64), b : Option2(I64)) { 1 }; f(Option2(I64).None2, Option2.Some2(1)) }
