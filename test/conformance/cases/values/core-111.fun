# recursive parameterized ADT match
{ type MyList(a) = Cons(a, MyList(a)) | Nil; match (Cons(1, Nil)) { Cons(x, _) => x, Nil => 0 } }
