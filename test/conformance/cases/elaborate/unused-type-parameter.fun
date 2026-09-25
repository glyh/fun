# a type former's parameter that does not occur in its body is an error at the
# declaration (ruling 2026-09-25) - the generative shape
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx;
    pub mk = fn() { table <- deref(table) + 1; Bx } } };
  b1 = Mk(());
  g = fn(x : b1.Box(I64)) { 1 };
  g(b1.mk()) }
