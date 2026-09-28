# a macro's output carries a pattern synonym use that supplies its type parameters
# (M.Two[I64, Bool](x, y)) through reflection and back. The second use's scrutinee
# type is a bare meta, so the supplied types are what solve the synonym's parameters:
# if reflection dropped them, that use would report them unsolved at the end of their
# scope. The `Option(I64)` supply is an application, not a path, so a slot that only
# held paths would refuse it.
{ M = module { pub pattern Two(a, b) = (a, b) };
  macro use(x) { x };
  f = fn(v) { use(match (v) { M.Two[Option(I64), Bool](p, q) => p }) };
  use(match ((1, True)) { M.Two[I64, Bool](p, q) => p }) + 6 }
