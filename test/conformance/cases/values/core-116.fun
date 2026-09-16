# qualified nested constructor pattern
{ A = module { pub B = module { pub type T = X(I64) | Y } }; open A; open B; match (X(7)) { X(n) => n, Y => 0 } }
