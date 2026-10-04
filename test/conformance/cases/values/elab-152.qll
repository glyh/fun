{ P = enum { Pair(I64, Option(I64)) }; open P; match (Pair(10, Some(4))) { Pair(a, None) => a, Pair(a, Some(b)) => a - b } }
