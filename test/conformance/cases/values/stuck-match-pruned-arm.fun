# a stuck match still reads back: it waits on the payload, arm 0 wins
# (the arm this case used to shadow is now an unreachable-arm error)
{ f = fn(x : Option(I64), y : match (Some(x)) { Some(Some(z)) => I64, None => I64, _ => Char }) { y }; f(Some(5), 5) }
