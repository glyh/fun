{ effect State(S) = sig { put : S -> Unit }; (fn(_) { perform State.put(True) } : Unit ->{State(I64)} Unit) }
