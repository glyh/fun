# qualified constructor alias pattern
{ S = module { pub type Color = Red | Green }; N = S; open N; match (Red) { Red => 1, Green => 2 } }
