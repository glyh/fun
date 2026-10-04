# bodyless infix applies same-named value
{
       myfst = fn(x, y) { x };
       infix (myfst);
       7 myfst 2
     }
