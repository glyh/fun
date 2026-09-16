# a fixity declaration attaches to the value of its name
{ twice = fn(x) { x * 2 }; prefix (twice); twice 21 }
