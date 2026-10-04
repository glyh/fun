# a closure capture compares as conversion does: b.T is acceptable where a.T is expected
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  a = mkset(); b = mkset();
  g = fn(x : a.T) { 1 };
  g(b.Leaf) }
