# a Decl parameter spliced into a quote
{
       M = module {
         macro with_extra(d : List(Decl)) : List(Decl) { quote { $d; pub extra = 22; } };
         with_extra({ pub x = 1; pub y = 10 })
       };
       M.x + M.y + M.extra
     }
