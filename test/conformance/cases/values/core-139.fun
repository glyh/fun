# match tuple literals
match (False, 1) { (True, x) => x, (False, _) => 9 }
