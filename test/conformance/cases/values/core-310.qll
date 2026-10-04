# identity survives the pipeline's re-evaluation: one declaration, three evaluations, one type
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  less = fn(a : I64, b : I64) { a < b };
  a = Set(I64, less);
  MkT = fn(c : I64 -> I64 -> Bool) { Set(I64, c).T };
  g = fn(x : a.T) { 1 };
  f = fn(t : Type) { match (t) { MkT(less) => 2, _ => 0 } };
  g(Set(I64, less).Leaf) + f(a.T) + f(Set(I64, less).T) * 10 }
