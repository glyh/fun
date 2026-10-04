open (import "std");
               syntax make : Decl { make $(n : Id) => { syntax $n { $n $x => $x * 2 } } };
               make double;
               pub r = double 21
