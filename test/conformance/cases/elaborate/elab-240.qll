{ effect State(S) = sig { get : Unit -> S; put : S -> Unit }; (fn(_) { match (perform State.get ()) { x => x, effect State.get () => 0 } } : Unit -> I64) }
