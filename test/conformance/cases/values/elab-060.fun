{ mk = fn(X : Type, junk : I64) { module { pub type T = A(X) | B } }; f = fn(t : mk(I64, 1).T) { 1 }; f(mk(I64, 2).B) }
