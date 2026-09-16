# syntax shadows syntax in a nested block
{ syntax t { t => 1 }; x = { syntax t { t => 2 }; t }; x * 10 + t }
