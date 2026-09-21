# a brace group separated from its callee is not an implicit argument
{ f = fn[n : I64](x : I64) { n }; f {7} }
