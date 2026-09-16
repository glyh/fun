{ mk = fn(X : Type) { type T = A(X) | B; fn(x : X) { match (A(x)) { A(n) => n, B => x } } }; mk(I64)(3) }
