{ M = module { effect State(S) = sig { get : Unit -> S } }; M.State(I64) }
