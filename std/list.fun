# Lists — what belongs here: everything a program does with a List, reached as
# `Std.Lists.f` or, after `open Std.Lists`, as a bare `f`. The module is this unit's
# own public bindings; the compiler names nothing here.
#
# Every operation is total: head, tail, nth and find answer an Option rather than
# aborting, take and drop saturate, and take/drop/range count from 0.
#
# Each public binding carries .NET XML doc comments (on `##` lines — a line comment,
# distinct from the ordinary `#` comment): <summary>, <returns>, and an <example>
# whose <code> line ends in "// returns V", the value the expression must evaluate to.
# test/run-doc-examples.sh runs every such line through the conformance runner
# and compares against V, so the examples are tested behaviour.
Lib = import "std/lib";
open Lib;

## <summary>Counts xs's elements; 0 for Nil.</summary>
## <returns>the number of elements in xs.</returns>
## <example>
## <code>Std.Lists.length(Cons(1, Cons(2, Cons(3, Nil)))) // returns 3</code>
## </example>
pub length = fn[A : Type](xs : List(A)) : I64 {
  rec go = fn(xs : List(A), acc : I64) : I64 {
    match (xs) { Nil => acc, Cons(_, t) => go(t, acc + 1) }
  };
  go(xs, 0)
};

## <summary>Reverses xs; Nil for Nil.</summary>
## <returns>the elements of xs, last first.</returns>
## <example>
## <code>Std.Lists.reverse(Cons(1, Cons(2, Cons(3, Nil)))) == Cons(3, Cons(2, Cons(1, Nil))) // returns True</code>
## </example>
pub reverse = fn[A : Type](xs : List(A)) : List(A) {
  rec go = fn(xs : List(A), acc : List(A)) : List(A) {
    match (xs) { Nil => acc, Cons(h, t) => go(t, Cons(h, acc)) }
  };
  go(xs, Nil)
};

## <summary>Appends ys to xs; either side may be Nil.</summary>
## <returns>the elements of xs followed by the elements of ys.</returns>
## <example>
## <code>Std.Lists.append(Cons(1, Cons(2, Nil)), Cons(3, Nil)) == Cons(1, Cons(2, Cons(3, Nil))) // returns True</code>
## </example>
pub append = fn[A : Type](xs : List(A), ys : List(A)) : List(A) {
  rec go = fn(xs : List(A)) : List(A) {
    match (xs) { Nil => ys, Cons(h, t) => Cons(h, go(t)) }
  };
  go(xs)
};

## <summary>Applies f to each element, in order; Nil for Nil.</summary>
## <returns>the transformed elements, in order.</returns>
## <example>
## <code>Std.Lists.map(fn(x) { x + 1 }, Cons(1, Cons(2, Nil))) == Cons(2, Cons(3, Nil)) // returns True</code>
## </example>
pub map = fn[A : Type, B : Type](f : A -> B, xs : List(A)) : List(B) {
  rec go = fn(xs : List(A)) : List(B) {
    match (xs) { Nil => Nil, Cons(h, t) => Cons(f(h), go(t)) }
  };
  go(xs)
};

## <summary>Keeps the elements p answers True for, in order; Nil for Nil.</summary>
## <returns>the elements of xs with p holding, in order.</returns>
## <example>
## <code>Std.Lists.filter(fn(x) { x > 1 }, Cons(1, Cons(2, Cons(3, Nil)))) == Cons(2, Cons(3, Nil)) // returns True</code>
## </example>
pub filter = fn[A : Type](p : A -> Bool, xs : List(A)) : List(A) {
  rec go = fn(xs : List(A)) : List(A) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (p(h)) { True => Cons(h, go(t)), False => go(t) }
    }
  };
  go(xs)
};

## <summary>Folds f over xs from the left, z first: f(…f(f(z, x0), x1)…); z for Nil.</summary>
## <returns>the final accumulator.</returns>
## <example>
## <code>Std.Lists.fold(fn(acc, x) { acc + x }, 0, Cons(1, Cons(2, Cons(3, Nil)))) // returns 6</code>
## </example>
pub fold = fn[A : Type, B : Type](f : B -> A -> B, z : B, xs : List(A)) : B {
  rec go = fn(xs : List(A), acc : B) : B {
    match (xs) { Nil => acc, Cons(h, t) => go(t, f(acc, h)) }
  };
  go(xs, z)
};

## <summary>Concatenates xss's lists; Nil for Nil and for a list of Nil.</summary>
## <returns>the elements of xss's lists in order.</returns>
## <remarks>Each append copies the prefix it already has, so the total is quadratic in
# the result's length.</remarks>
## <example>
## <code>Std.Lists.concat(Cons(Cons(1, Cons(2, Nil)), Cons(Cons(3, Nil), Nil))) == Cons(1, Cons(2, Cons(3, Nil))) // returns True</code>
## </example>
pub concat = fn[A : Type](xss : List(List(A))) : List(A) {
  fold(fn(acc : List(A), xs : List(A)) : List(A) { append(acc, xs) }, Nil, xss)
};

## <summary>Finds the first element p answers True for.</summary>
## <returns>that element as an option: None for Nil or when p holds for none of them.</returns>
## <example>
## <code>Std.Lists.find(fn(x) { x > 1 }, Cons(1, Cons(2, Cons(3, Nil)))) == Some(2) // returns True</code>
## </example>
pub find = fn[A : Type](p : A -> Bool, xs : List(A)) : Option(A) {
  rec go = fn(xs : List(A)) : Option(A) {
    match (xs) {
      Nil => None,
      Cons(h, t) => match (p(h)) { True => Some(h), False => go(t) }
    }
  };
  go(xs)
};

## <summary>Takes xs's first element.</summary>
## <returns>the first element as an option: None for Nil.</returns>
## <example>
## <code>Std.Lists.head(Cons(1, Cons(2, Nil))) == Some(1) // returns True</code>
## </example>
pub head = fn[A : Type](xs : List(A)) : Option(A) {
  match (xs) { Nil => None, Cons(h, _) => Some(h) }
};

## <summary>Drops xs's first element.</summary>
## <returns>xs without its first element, as an option: None for Nil.</returns>
## <example>
## <code>Std.Lists.tail(Cons(1, Cons(2, Nil))) == Some(Cons(2, Nil)) // returns True</code>
## </example>
pub tail = fn[A : Type](xs : List(A)) : Option(List(A)) {
  match (xs) { Nil => None, Cons(_, t) => Some(t) }
};

## <summary>Takes xs's element at 0-based index i.</summary>
## <returns>that element as an option: None for i < 0, for i past the end and for Nil.</returns>
## <example>
## <code>Std.Lists.nth(1, Cons(1, Cons(2, Nil))) == Some(2) // returns True</code>
## </example>
pub nth = fn[A : Type](i : I64, xs : List(A)) : Option(A) {
  rec go = fn(i : I64, xs : List(A)) : Option(A) {
    match (i64_to_bool(lt_i64(i, 0))) {
      True => None,
      False => match (xs) {
        Nil => None,
        Cons(h, t) => match (i64_to_bool(eq_i64(i, 0))) {
          True => Some(h),
          False => go(i - 1, t)
        }
      }
    }
  };
  go(i, xs)
};

## <summary>Takes xs's first element, with a fallback.</summary>
## <returns>xs's first element, or d for Nil.</returns>
## <example>
## <code>Std.Lists.head_or(0, Nil) // returns 0</code>
## </example>
pub head_or = fn[A : Type](d : A, xs : List(A)) : A {
  match (xs) { Nil => d, Cons(h, _) => h }
};

## <summary>Takes xs's element at 0-based index i, with a fallback.</summary>
## <returns>the element at index i, or d when nth finds none (a negative or past-the-end
# index, or Nil).</returns>
## <example>
## <code>Std.Lists.nth_or(0, 9, Cons(1, Nil)) // returns 0</code>
## </example>
pub nth_or = fn[A : Type](d : A, i : I64, xs : List(A)) : A {
  match (nth(i, xs)) { Some(x) => x, None => d }
};

## <summary>Takes xs's first n elements.</summary>
## <returns>the first n elements of xs, or all of xs when n is past the end; Nil for
# n <= 0 and for Nil.</returns>
## <example>
## <code>Std.Lists.take(2, Cons(1, Cons(2, Cons(3, Nil)))) == Cons(1, Cons(2, Nil)) // returns True</code>
## </example>
pub take = fn[A : Type](n : I64, xs : List(A)) : List(A) {
  rec go = fn(n : I64, xs : List(A)) : List(A) {
    match (i64_to_bool(le_i64(n, 0))) {
      True => Nil,
      False => match (xs) { Nil => Nil, Cons(h, t) => Cons(h, go(n - 1, t)) }
    }
  };
  go(n, xs)
};

## <summary>Drops xs's first n elements.</summary>
## <returns>xs without its first n elements, or Nil when n is past the end; xs for
# n <= 0 and Nil for Nil.</returns>
## <example>
## <code>Std.Lists.drop(2, Cons(1, Cons(2, Cons(3, Nil)))) == Cons(3, Nil) // returns True</code>
## </example>
pub drop = fn[A : Type](n : I64, xs : List(A)) : List(A) {
  rec go = fn(n : I64, xs : List(A)) : List(A) {
    match (i64_to_bool(le_i64(n, 0))) {
      True => xs,
      False => match (xs) { Nil => Nil, Cons(_, t) => go(n - 1, t) }
    }
  };
  go(n, xs)
};

## <summary>Pairs xs's elements with ys's, in order, truncating at the shorter list.</summary>
## <returns>the pairs of elements at each index; Nil when either side is Nil.</returns>
## <example>
## <code>match (Std.Lists.zip(Cons(1, Nil), Cons(10, Nil))) { Cons((a, b), _) => a + b, Nil => 0 } // returns 11</code>
## </example>
pub zip = fn[A : Type, B : Type](xs : List(A), ys : List(B)) : List(Tuple(2, A, B)) {
  rec go = fn(xs : List(A), ys : List(B)) : List(Tuple(2, A, B)) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (ys) { Nil => Nil, Cons(h2, t2) => Cons((h, h2), go(t, t2)) }
    }
  };
  go(xs, ys)
};

## <summary>Applies f to xs's and ys's elements pairwise, truncating at the shorter list.</summary>
## <returns>the combined elements; Nil when either side is Nil.</returns>
## <example>
## <code>Std.Lists.zip_with(fn(a, b) { a + b }, Cons(1, Cons(2, Nil)), Cons(10, Cons(20, Nil))) == Cons(11, Cons(22, Nil)) // returns True</code>
## </example>
pub zip_with = fn[A : Type, B : Type, C : Type](f : A -> B -> C, xs : List(A), ys : List(B)) : List(C) {
  rec go = fn(xs : List(A), ys : List(B)) : List(C) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (ys) { Nil => Nil, Cons(h2, t2) => Cons(f(h, h2), go(t, t2)) }
    }
  };
  go(xs, ys)
};

## <summary>Says whether p answers True for any element.</summary>
## <returns>True when p holds for at least one element; False for Nil.</returns>
## <example>
## <code>Std.Lists.any(fn(x) { x > 1 }, Cons(1, Cons(2, Nil))) // returns True</code>
## </example>
pub any = fn[A : Type](p : A -> Bool, xs : List(A)) : Bool {
  rec go = fn(xs : List(A)) : Bool {
    match (xs) {
      Nil => False,
      Cons(h, t) => match (p(h)) { True => True, False => go(t) }
    }
  };
  go(xs)
};

## <summary>Says whether p answers True for every element.</summary>
## <returns>True when p holds for every element; True for Nil.</returns>
## <example>
## <code>Std.Lists.all(fn(x) { x > 0 }, Cons(1, Cons(2, Nil))) // returns True</code>
## </example>
pub all = fn[A : Type](p : A -> Bool, xs : List(A)) : Bool {
  rec go = fn(xs : List(A)) : Bool {
    match (xs) {
      Nil => True,
      Cons(h, t) => match (p(h)) { True => go(t), False => False }
    }
  };
  go(xs)
};

## <summary>Builds the 0-based indices below n, 0 first.</summary>
## <returns>the numbers from 0 to n - 1; Nil for n <= 0.</returns>
## <example>
## <code>Std.Lists.range(4) == Cons(0, Cons(1, Cons(2, Cons(3, Nil)))) // returns True</code>
## </example>
pub range = fn(n : I64) : List(I64) {
  rec go = fn(i : I64) : List(I64) {
    match (i64_to_bool(lt_i64(i, n))) { True => Cons(i, go(i + 1)), False => Nil }
  };
  go(0)
};

# list_eq_go is the element-wise Eq for two lists, generic in the element type; it is
# True for two Nils and False when one side is Nil and the other is not.
rec list_eq_go : [B : Eq] -> List(B) -> List(B) -> Bool = fn[B : Type](xs, ys) {
  match (xs) {
    Nil => match (ys) { Nil => True, Cons(_, _) => False },
    Cons(h, t) => match (ys) {
      Nil => False,
      Cons(h2, t2) => match (Eq.eq(h, h2)) { True => list_eq_go[B](t, t2), False => False }
    }
  }
};

## <summary>Compares two lists with the element type's Eq.</summary>
## <returns>True for two Nils and for lists whose elements are pairwise equal; False when
# the lengths differ or an element differs.</returns>
## <example>
## <code>Cons(1, Cons(2, Nil)) == Cons(1, Cons(2, Nil)) // returns True</code>
## </example>
pub impl list_eq[a : Eq] : Eq(List(a)) = module {
  fn eq(xs, ys) { list_eq_go(xs, ys) }
};
