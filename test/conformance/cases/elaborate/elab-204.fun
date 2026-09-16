{ effect IO = sig { read : Unit -> I64 }; (fn(x) { x } : I64 ->{IO} I64) }
