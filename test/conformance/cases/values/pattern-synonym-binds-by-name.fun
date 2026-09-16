# a pattern synonym binds its arguments by parameter name: first is a, which sits in the second slot
{ M = module { pub Point = enum { Pt(I64, I64) }; open Point; pub pattern Flip(a, b) = Pt(b, a) }; match (M.Point.Pt(10, 20)) { M.Flip(first, second) => first } }
