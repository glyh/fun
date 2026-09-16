{ mk = fn(X : Type) { module { pub y = True; pub f = fn(x : X) { x } } }; mk(I64).f(7) }
