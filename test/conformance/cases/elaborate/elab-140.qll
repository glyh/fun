{ Apply = fn(F, M : sig { x : I64 }) { F(M) }; Apply(fn(M : sig { x : I64 }) { module { pub z = M.x } })(module { pub x = 1 }) }
