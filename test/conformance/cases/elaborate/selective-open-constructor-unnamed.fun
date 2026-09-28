# listing R does not bring G
{ M = module { pub rec Color = enum { R, G }; export Color }; open M.{R}; match (G) { R => 1, _ => 2 } }
