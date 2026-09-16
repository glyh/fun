{ effect State(S) = sig { get : Unit -> S }; effect IO = sig { read : Unit -> I64 }; ((fn(x) { x } : I64 ->{IO, State(I64)} I64) : I64 ->{State(I64), IO} I64) }
