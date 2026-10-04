# a transitive order
{ order low; order mid : stronger_than(low); order high : stronger_than(mid);
       infix (<+>) low ($a, $b) { $a + $b }; infix (<*>) high ($a, $b) { $a * $b };
       1 <+> 2 <*> 3 }
