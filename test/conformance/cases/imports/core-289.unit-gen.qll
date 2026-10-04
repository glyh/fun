open (import "std");
               syntax make_inc : Decl {
               make_inc $(n : Id) =>
                   {
                     syntax $n { $n $x => $x + 1 }
                   }
               };
               make_inc inc;
               pub result = inc 5
