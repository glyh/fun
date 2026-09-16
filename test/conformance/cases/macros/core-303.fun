# a Decl form binds for the rest of a block
{
       syntax seven : Decl { seven $(n : Id) => { $n = 7 } };
       seven x;
       x
     }
