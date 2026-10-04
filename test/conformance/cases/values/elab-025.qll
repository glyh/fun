{ M = module { pub type T = B }; f = fn(T : Type, X : Type) { { open M; match (X) { T => 1, _ => 0 } } }; f(I64, M.T) }
