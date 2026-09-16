# imported nested constructor pattern
{ N = import "nested"; match (N.M.X(7)) { N.M.X(n) => n, N.M.Y => 0 } }
