# an import cycle is an error
{ A = import "a"; A.x }
