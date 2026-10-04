# |> chains left to right: x |> f |> g is g(f(x))
{ inc = fn(x) { x + 1 }; double = fn(x) { x * 2 }; 3 |> inc |> double }
