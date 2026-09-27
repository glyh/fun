# a closure capture compares as conversion does: a type-case says a.T and b.T are one type
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  a = mkset(); b = mkset();
  f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
  f(b.T) }
