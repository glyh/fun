# an unused synonym whose right-hand side is ill-typed errors at the declaration
{ M = module { pub P = enum { Pair(I64, Bool) }; open P; pub pattern Bad(a) = Pair(a, a) }; 1 }
