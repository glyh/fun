# a binder's type may apply a former-valued binder over a parameterized nominal;
# the former must use its parameter (ruling 2026-09-25), so it dispatches on it
{ type O(X) = N | S(X); h = fn(G : O(I64) -> Type, v : G(N)) { { z = v; 1 } }; h(fn(o : O(I64)) { match (o) { N => I64, S(_) => Char } }, 5) }
