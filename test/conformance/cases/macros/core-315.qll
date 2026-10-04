# an Id parameter binds for the caller
{
       M = module {
         macro seven(n : Id) : Decl { Syntax.decl_let(n, Syntax.i64(7), False) };
         seven(x);
         pub r = x
       };
       M.r
     }
