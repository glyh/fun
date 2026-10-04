{ type Inner = X I64 | Y I64; type Outer = A Inner | B Inner; match (A(X(1))) { A(X(x)) => x, B(X(x)) => x } }
