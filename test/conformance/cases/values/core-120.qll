# qualified constructor sharing its type name
{ M = module { pub type T = T(I64) | Y }; match (M.T(7)) { M.T(n) => n, M.Y => 0 } }
