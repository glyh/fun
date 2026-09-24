# a macro that builds a two-parameter trait: a trait takes exactly one parameter
{
  macro m(n : Id, a : Id, b : Id) {
    Syntax.RawTraitDef(None, n, Cons(a, Cons(b, Nil)), Nil, Syntax.i64(1))
  };
  m(P, A, B)
}
