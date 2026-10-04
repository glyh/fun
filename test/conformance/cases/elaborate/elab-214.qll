{ effect State(S) = sig { get : Unit -> S }; f : Unit ->{State(I64)} I64 = fn(_) { perform State.get () }; (fn(_) { f() } : Unit -> I64) }
