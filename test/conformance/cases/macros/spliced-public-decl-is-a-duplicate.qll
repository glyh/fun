# a declaration spliced twice is a duplicate member when it declares a public one
{
       M = module {
         macro twice_decls(d : List(Decl)) : List(Decl) { quote { $d; $d } };
         twice_decls({ pub x = 1 })
       };
       M.x
     }
