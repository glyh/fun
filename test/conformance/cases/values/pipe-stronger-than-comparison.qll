# |> binds tighter than comparison (Elixir-style): x |> f == y is (x |> f) == y
{ inc = fn(x) { x + 1 }; 3 |> inc == 4 }
