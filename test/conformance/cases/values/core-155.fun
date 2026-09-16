# record pattern renamed field
{ Point = struct {x: I64; y: I64}; match (Point{x = 10; y = 20}) { Point {x = wow; y} => wow + y } }
