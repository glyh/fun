# constructor sharing its type name
{ type T = T(I64) | Y; match (T(7)) { T(n) => n, Y => 0 } }
