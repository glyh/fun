# module signature functor
{ F = fn(M : sig { x : I64 }) { module { pub doubled = M.x + M.x } }; F(module { pub x = 21 }).doubled }
