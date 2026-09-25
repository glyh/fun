# a positive control: a former whose parameter occurs in its body is fine, and a
# lambda whose result is a value is an ordinary function of a type argument
{ M = module { pub type Box(A) = Bx(A); pub mk = fn() { Bx(0) } };
  g = fn(x : M.Box(I64)) { 1 };
  at = fn(A : Type) { 1 };
  g(M.mk()) + at(I64) }
