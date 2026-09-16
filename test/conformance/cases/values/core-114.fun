# constructor tuple payload remains single arg
{ type Pair = P(Tuple(2, I64, Bool)); match (P((1, True))) { P(pair) => pair.0 } }
