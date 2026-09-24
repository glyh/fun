# a match on a known scrutinee's unknown part waits (it does not take the default arm)
{ f = fn(x : Option(I64), y : match (Some(x)) { Some(Some(z)) => I64, _ => Char }) { y }; f(Some(5), 5) }
