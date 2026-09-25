# a stuck match whose tree prunes a shadowed arm still reads back: it waits, arm 0 wins
{ f = fn(x : Option(I64), y : match (Some(x)) { Some(a) => I64, Some(Some(z)) => I64, _ => Char }) { y }; f(Some(5), 5) }
