{ type O(X) = N | S(X); h = fn(G : O(I64) -> Type, v : G(N)) { { z = v; 1 } }; h(fn(o : O(I64)) { I64 }, 5) }
