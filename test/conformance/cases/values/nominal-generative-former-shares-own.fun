# E11: a parametric nominal in a generative module is usable with itself
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx(A);
    pub mk = fn(A : Type, a : A) { table <- deref(table) + 1; Bx(a) } } };
  b1 = Mk(());
  g = fn(x : b1.Box(I64)) { 1 };
  g(b1.mk(I64, 3)) }
