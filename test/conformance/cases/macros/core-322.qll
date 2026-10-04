# a Decl parameter's items arrive unread
{
       macro unread(d : List(Decl)) {
         match (d) { Cons(Syntax.DeclItems(_), Nil) => Syntax.i64(1), _ => Syntax.i64(0) }
       };
       unread({ a = 1; b = 2 })
     }
