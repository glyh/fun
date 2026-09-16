# export an enum's constructors
{ N = module { pub rec T = enum { A, B }; export T }; open N; match (B) { A => 1, B => 2 } }
