# imported record pattern
{ S = import "shapes"; match (S.Point{x = 1; y = 2}) { S.Point {x; y} => x + y } }
