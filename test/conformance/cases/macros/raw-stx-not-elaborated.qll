# a macro emitting the syntax-object marker; the elaborator refuses it
{ macro m(_) { Syntax.RawStx(None, Syntax.i64(7)) }; m(0) }
