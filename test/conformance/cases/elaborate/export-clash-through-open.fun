# a constructor exported from an enum reached through open clashes with a public member of its name
{ N = module { pub rec T = enum { T, U } }; M = module { pub T = 5; open N; export T }; 1 }
