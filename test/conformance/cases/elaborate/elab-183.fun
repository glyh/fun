{ type C = X I64 | Y I64; type B = P C | Q C; type A = M B | N B; match (M(P(X(1)))) { M(P(X(x))) => x } }
