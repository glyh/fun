# a pattern synonym use giving more arguments than the synonym takes
{ M = module { pub Point = enum { Pt(I64, I64) }; open Point; pub pattern First(a) = Pt(a, _) }; match (M.Point.Pt(3, 4)) { M.First(x, y) => x } }
