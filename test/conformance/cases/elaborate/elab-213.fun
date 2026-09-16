{ effect State(S) = sig { get : Unit -> S }; (fn(_) { 1 } : Unit ->{State(I64)} I64) }
