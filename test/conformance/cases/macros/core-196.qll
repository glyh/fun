# macro name shadowing regardless of kind
{
       macro m(_) : Decl { quote { x = 0 } };
       macro m(stx) { Syntax.i64(1) };
       m(0)
     }
