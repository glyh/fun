# no capture
{ macro m(_) { quote(fn(x) { x }) }; x = 1; (m(0))(x) }
