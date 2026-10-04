{ effect State(S) = sig { get : Unit -> S }; (fn(_) { perform State.missing () } : Unit ->{State(I64)} I64) }
