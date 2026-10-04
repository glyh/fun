# a constructor may share the name of the enum it is exported from; the path denotes it
{ N = module { pub T = enum { T, U }; export T }; N.T }
