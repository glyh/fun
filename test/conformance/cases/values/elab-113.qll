{ rec Numbers = struct { head : I64; tail : Option(Numbers) }; l2 = Numbers{ head = 2, tail = Some(Numbers{ head = 40, tail = None }) }; match (l2.tail) { Some(x) => x.head + l2.head, None => 0 } }
