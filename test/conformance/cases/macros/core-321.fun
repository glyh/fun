# a Decl parameter spliced twice
{
       M = module {
         macro twice_decls(d : List(Decl)) : List(Decl) { quote { $d; $d } };
         twice_decls({ pub x = 1; pub y = 2 })
       };
       M.y
     }
