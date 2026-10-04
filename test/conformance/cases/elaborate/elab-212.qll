{ effect State(S) = sig { get : Unit -> S }; (fn(_) { perform State.get () } : Unit -> I64) }
