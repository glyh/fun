# expanded items placed back into a quote
{
       M = module {
         macro wrap(d : List(Decl)) : List(Decl) { e = Syntax.expand_decls(d); quote { $e; pub z = 5; } };
         wrap({ syntax inc { inc $x => $x + 1 }; pub y = inc 1 })
       };
       M.y + M.z
     }
