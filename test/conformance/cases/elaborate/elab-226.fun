{ effect State(S) = sig { get : Unit -> S }; f : Unit ~> I64 = fn(_) { perform State.get () }; 1 }
