# qualified record pattern
{ M = module { pub Point = struct { x: I64; y: I64; } }; open M; match (Point{x = 1; y = 2}) { Point {x; y} => x + y } }
