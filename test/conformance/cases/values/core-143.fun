# record construction field order
{ Point = struct {x: I64; y: I64}; p = Point{y = 20; x = 10}; p.x + p.y }
