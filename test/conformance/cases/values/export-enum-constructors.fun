# an enum's constructors are exported as members
{ N = module { pub T = enum { A, B }; export T }; N.B }
