# a pattern synonym declared in a module resolves through open like any other name
{ M = module { pub P = enum { Pair(I64, Char) }; open P; pub pattern Swap(c, n) = Pair(n, c) }; open M; match (P.Pair(7, 'a')) { Swap(x, y) => y } }
