{ Stack = sig { T : Type; empty : T }; Bad = module { pub T = I64; pub empty = True }; count = fn(s : Stack) { 1 }; count(Bad) }
