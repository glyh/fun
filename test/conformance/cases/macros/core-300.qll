# a macro reads a block's tokens
{
       macro sql(q) {
         match (Syntax.tokens(q)) {
         Cons(Syntax.Tok(_, Syntax.IdentTok(word), _), _) =>
             if (i64_to_bool(eq_string(word, "SELECT"))) { Syntax.i64(1) } else { Syntax.i64(0) },
         _ => Syntax.i64(2)
         }
       };
       sql({ SELECT name FROM users WHERE age > 18 })
     }
