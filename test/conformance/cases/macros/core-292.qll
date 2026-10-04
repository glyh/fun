# a replacement reads roles as of its definition
{ infix (~) ($a, $b) { $a - $b }; syntax t { t => 1 ~ 2 };
       infix (~) ($a, $b) { $a + $b }; t * 10 + (1 ~ 2) }
