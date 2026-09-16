{ F = fn(M : sig { x : I64 }) { module { pub a = M.x } }; G = fn(N : sig { a : I64 }) { module { pub b = N.a } }; (G(F(module { pub x = 1 }))).b }
