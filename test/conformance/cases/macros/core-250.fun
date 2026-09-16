# a hole before a comma reads to it
{ syntax both { both ($a, $b) => $a + $b }; both (1 + 1, 3) }
