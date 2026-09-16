# lazy delta: two calls of one pure fixpoint on convertible arguments are equal without unfolding
{ rec loop : I64 -> I64 = fn(n) { loop(n) }; g = fn(F : I64 -> Type, n : I64, y : F(loop(n))) { (y : F(loop(n))) }; 2 }
