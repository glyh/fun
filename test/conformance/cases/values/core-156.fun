# record pattern partial
{ Point = struct {x: I64; y: I64}; match (Point{x = 3; y = 4}) { Point {x; _} => x } }
