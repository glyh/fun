# record pattern reordered
{ Point = struct { x: I64; y: I64; }; match (Point{x = 1; y = 2}) { Point {y; x} => x + y } }
