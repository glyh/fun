# imported ADT match
{ C = import "color"; match (C.default) { C.Red => 1, C.Green => 2, C.Blue => 3 } }
