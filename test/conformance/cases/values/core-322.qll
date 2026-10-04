# the negative control: two capture lambdas whose bodies differ stay different types
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  mkset2 = fn() { Set(I64, fn(x : I64, y : I64) { x <= y }) };
  a = mkset(); b = mkset2();
  f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
  f(b.T) }
