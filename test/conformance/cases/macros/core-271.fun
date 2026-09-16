# generated syntax form can reuse outer hole
{
       syntax make_adder {
       make_adder $base => {
           syntax add_base { add_base $x => $x + $base };
           add_base 5 + add_base 1
         }
       };
       make_adder 5
     }
