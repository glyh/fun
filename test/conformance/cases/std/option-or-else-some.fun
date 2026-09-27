# or_else keeps the option when it holds a value
Std.Options.get_or(9, Std.Options.or_else(None[I64], Some(41)))
