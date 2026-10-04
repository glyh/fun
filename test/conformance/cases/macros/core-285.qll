# a List(Decl) hole takes a group of declarations
{ M = module { syntax both : Decl { both $(ds : List(Decl)) => { $ds } }; both { pub a = 1; pub b = 2 } }; M.a + M.b }
