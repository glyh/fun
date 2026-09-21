# a brace implicit argument on a value of unknown type, then an explicit one
{ h = fn(g) { g{7}(0) }; h(fn[n : I64](x : I64) { n }) }
