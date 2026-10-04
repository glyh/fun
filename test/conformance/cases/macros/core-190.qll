# infix macro expands
{
       infix (~) (stx) { Syntax.i64(9) };
       1 ~ 2
       }
