# a brace implicit argument is bound and the result depends on its value
{ f = fn[n : I64](x : I64) { n }; f{7}(0) }
