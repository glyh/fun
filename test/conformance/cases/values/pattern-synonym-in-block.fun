# a pattern synonym declared in a block, like any other declaration
{ P = enum { Pair(I64, Char) }; open P; pattern Swap(c, n) = Pair(n, c); match (Pair(1, 'x')) { Swap(a, b) => b } }
