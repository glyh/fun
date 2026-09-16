{ effect State(S) = sig { put : S -> Unit }; (fn(_) { perform State.put(42) } : Unit ->{State(I64)} Unit) }
