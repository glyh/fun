# an order group is named through the unit that exports it
{ G = import "g"; order mine : stronger_than(G.tight); infix (<+>) mine ($a, $b) { $a }; 1 <+> 2 }
