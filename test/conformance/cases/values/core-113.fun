# constructor comma payload distinct from tuple
{ type Triple(a, b, c) = T(a, Tuple(2, b, c)); match (T(1, (2, 3))) { T(x, yz) => x + yz.0 + yz.1 } }
