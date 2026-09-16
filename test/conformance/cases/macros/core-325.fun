# a later item reads with syntax an earlier item declares
{
       M = module {
         macro keep(d : List(Decl)) : List(Decl) { Syntax.expand_decls(d) };
         keep({ syntax inc { inc $x => $x + 1 }; pub y = inc 1 })
       };
       M.y
     }
