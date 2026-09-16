# open imported module exposes constructors
{ C = import "color"; open C; match (Red) { Red => 1, Green => 2 } }
