# match constructor or-pattern binding
{ type E = A(I64) | B(I64); match (B(5)) { (A(x) | B(x)) => x } }
