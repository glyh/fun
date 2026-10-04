# imported module alias pattern
{ C = import "color"; Alias = C; match (C.Red) { Alias.Red => 1, Alias.Green => 2 } }
