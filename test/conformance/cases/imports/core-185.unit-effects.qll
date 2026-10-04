pub effect State(S) = sig { get : Unit -> S }; pub read : Unit ->{State(I64)} I64 = fn(_) { perform State.get(()) }
