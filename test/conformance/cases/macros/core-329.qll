# a : Decl macro returns one quoted declaration
{ M = module { macro one() : Decl { quote { pub a = 1 } }; one() }; M.a }
