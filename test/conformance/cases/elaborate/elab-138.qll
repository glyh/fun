{ F = fn(M : sig { x : I64 }) { module { pub y = M.x } }; A = module { pub x = 1 }; B = F(A); B.y }
