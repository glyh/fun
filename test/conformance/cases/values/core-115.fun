# qualified constructor pattern
{ S = module { pub type Color = Red | Green }; open S; match (Green) { Red => 1, Green => 2 } }
