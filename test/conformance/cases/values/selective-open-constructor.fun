# open M.{R} brings the named constructor exported into M
{ M = module { pub rec Color = enum { R, G }; export Color }; open M.{R}; match (R) { R => 1, _ => 2 } }
