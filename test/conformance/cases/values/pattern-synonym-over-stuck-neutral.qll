# a pattern synonym whose constructor's payload type is a *stuck* neutral: g(T) is a
# type-level match on the variable T, so the synonym's scrutinee type is unknown and
# its types must generalise over it (prototype: elab_generalize.ml collects through
# any value; the port's collector used to refuse the VNeutral outright)
{ g = fn(T : Type) { match (T) { I64 => String, _ => I64 } };
  E = fn(T : Type) { enum { C(g(T)) } };
  M = module { pub pattern P(a) = E.C(a) };
  1 }
