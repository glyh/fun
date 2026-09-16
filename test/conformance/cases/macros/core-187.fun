# no capture
{ x = 1; macro m(_) { quote(fn(x) { x }) }; (m(0))(x) }
