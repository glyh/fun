# record pattern shorthand
{ Point = struct { x: I64; y: I64; }; match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }
