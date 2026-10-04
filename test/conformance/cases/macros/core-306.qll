# a quote fills a hole naming generated syntax
{
       M = module {
         macro make(n) : List(Decl) {
           match (n) { Syntax.Var(name) => quote { syntax $name { $name $x => $x * 2 }; }, _ => quote { } }
         };
         make(double);
         pub r = double 21
       };
       M.r
     }
