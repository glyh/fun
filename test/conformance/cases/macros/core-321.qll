# a Decl parameter spliced twice: the bindings are private, so no member is duplicated
{
       M = module {
         macro twice_decls(d : List(Decl)) : List(Decl) { quote { $d; $d } };
         twice_decls({ x = 1; y = 2 });
         pub r = y
       };
       M.r
     }
