# Pattern builder evaluates
{
       macro check(_) { Syntax.i64(1) };
       { _ = Syntax.pat_wild; check(0) }
     }
