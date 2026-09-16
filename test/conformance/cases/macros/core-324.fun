# expand_decls reads the items, so a macro can count them
{
       macro count(d : List(Decl)) {
         match (Syntax.expand_decls(d)) { Cons(_, Cons(_, Cons(_, Nil))) => Syntax.i64(3), _ => Syntax.i64(0) }
       };
       count({ a = 1; b = 2; c = 3 })
     }
