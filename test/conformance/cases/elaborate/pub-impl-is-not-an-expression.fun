# a program is an expression; a `pub impl` is a unit item, not an expression
pub impl Eq(I64) = module { eq = fn(x, y) { True } }
