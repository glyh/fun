# opened constructors usable in later bindings
{ M = import "user"; match (M.v) { Green => 2, Red => 1 } }
