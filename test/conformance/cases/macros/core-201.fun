# { x = 1; macro m(e) { quote((fn(x) { $e })(2)) }; y : I64 = m(x); y }
{ x = 1; macro m(e) { quote((fn(x) { $e })(2)) }; y : I64 = m(x); y }
