# E11: a type-case tells two evaluations of a generative former apart, by stamp
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx(A);
    pub mk = fn(A : Type, a : A) { table <- deref(table) + 1; Bx(a) } } };
  b1 = Mk(()); b2 = Mk(());
  f = fn(t : Type) { match (t) { b1.Box(I64) => 1, _ => 0 } };
  f(b1.Box(I64)) * 10 + f(b2.Box(I64)) }
