# E11: a parametric nominal in a generative module is unique per evaluation
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx(A);
    pub mk = fn(A : Type, a : A) { table <- deref(table) + 1; Bx(a) } } };
  b1 = Mk(()); b2 = Mk(());
  g = fn(x : b1.Box(I64)) { 1 };
  g(b2.mk(I64, 3)) }
