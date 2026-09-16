# module public function
{ M = module { helper = fn(x) { x * 2 }; pub double = helper }; M.double(21) }
