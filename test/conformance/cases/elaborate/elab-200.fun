{ effect State(S) = sig { get : Unit -> S }; effect Env(S) = sig { get : Unit -> S }; ((fn(x) { x } : Unit ->{State(I64)} Unit) : Unit ->{Env(I64)} Unit) }
