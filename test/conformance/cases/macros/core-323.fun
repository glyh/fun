# a Decl parameter is one declaration, returned as a Decl
{
       M = module {
         macro keep1(d : Decl) : Decl { d };
         keep1({ pub x = 4 })
       };
       M.x
     }
