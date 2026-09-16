# struct impl for Self
{ Point = struct { x: I64; pub impl Eq(Self) = module { fn eq(lhs, rhs) { lhs.x == rhs.x } } }; Point{x = 1} == Point{x = 1} }
