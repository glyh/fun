# imported record pattern alias
{ S = import "shapes"; Alias = S; match (S.Point{x = 1; y = 2}) { Alias.Point {x; y} => x + y } }
