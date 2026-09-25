# a binder's type may apply a former-valued binder; the former must use its
# parameter (ruling 2026-09-25), so it dispatches on the value it is given
{ type T = A(I64) | B; h = fn(G : T -> Type, v : G(A(1))) { { z = v; 1 } }; h(fn(t : T) { match (t) { A(_) => I64, B => Char } }, 5) }
