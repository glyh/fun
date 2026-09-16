# a quote's inner rule binds its own holes, the macro's fill the rest
{
       M = module {
         macro make_adder(base) : List(Decl) {
           quote { syntax add_base { add_base $x => $x + $base }; pub result = add_base 1 + add_base 10; }
         };
         make_adder(5)
       };
       M.result
     }
