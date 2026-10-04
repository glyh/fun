{ Point = struct { x: I64; }; match (Point{x = 1}) { Point {y; _} => y } }
