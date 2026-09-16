# both splice at calls in one module
{ M = module {
         macro one() : Decl { quote { pub c = 1 } };
         macro two() : List(Decl) { quote { pub a = 1; pub b = 2 } };
         one(); two()
       }; M.a + M.b + M.c }
