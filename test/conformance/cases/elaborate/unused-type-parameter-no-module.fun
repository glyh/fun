# the same refusal with no generative module: the rule is about the declaration,
# not about sealing (ruling 2026-09-25)
{ M = module { pub type Box(A) = Bx; pub mk = fn() { Bx } };
  g = fn(x : M.Box(I64)) { 1 };
  g(M.mk()) }
