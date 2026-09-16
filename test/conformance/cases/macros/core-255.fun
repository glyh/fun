# an ungrouped operator is weaker than a grouped one
{ infix (<>) ($a, $b) { $a * $b }; 1 + 2 <> 3 }
