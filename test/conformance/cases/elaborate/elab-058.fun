{ mk = fn(X : Type) { module { pub type T = A(X) | B } }; f = fn(t : mk(I64).T) { 1 }; f(mk(Bool).B) }
