# repeated import
{ A = import "m"; B = import "m"; A.x + B.x }
