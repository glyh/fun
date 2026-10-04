# operator shadowing
{
       syntax choose { choose => 1 };
       ignored = {
         syntax choose { choose => 2 };
         choose
       };
       choose
      }
