# the negative control: two capture lambdas with the same body but different captures stay different types
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn(n : I64) { Set(I64, fn(x : I64, y : I64) { n < y }) };
  a = mkset(0); b = mkset(1);
  f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
  f(b.T) }
