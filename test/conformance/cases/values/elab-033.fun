{ type T = A(I64) | B; h = fn(G : T -> Type, v : G(A(1))) { { z = v; 1 } }; h(fn(t : T) { I64 }, 5) }
