# a : List(Decl) macro returns several
{ M = module { macro two() : List(Decl) { quote { pub a = 1; pub b = 2 } }; two() }; M.a + M.b }
