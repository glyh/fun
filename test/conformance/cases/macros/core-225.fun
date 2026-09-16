# bodyless prefix applies same-named value
{
       ident = fn(x) { x };
       prefix (ident);
       ident 5
     }
