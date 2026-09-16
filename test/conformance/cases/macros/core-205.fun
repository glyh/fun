# quoted syntax parses where written
{ macro m(e) { quote(match ($e) { True => 1, False => 0 }) }; m(True) }
