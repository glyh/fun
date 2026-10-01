# an export selection takes the last member of a name: the constructor sharing its enum's name (I3), not the nominal
{ N = module { pub rec T = enum { T(I64), Y }; export T }; M = module { export N.{T} }; M.T(3) }
