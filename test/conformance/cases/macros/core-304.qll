# expanded forms placed back into output
{
       syntax double { double $x => $x + $x };
       macro pre(b) { Syntax.expand_block(b) };
       pre({ z = 4; double z })
     }
