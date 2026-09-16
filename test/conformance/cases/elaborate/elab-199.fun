{ effect State(S) = sig { get : Unit -> S }; ((fn(x) { x } : Unit ->{State(I64)} Unit) : Unit ->{State(I64)} Unit) }
