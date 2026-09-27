# Lists — what belongs here: everything a program does with a List, reached as
# `Std.Lists.f` or, after `open Std.Lists`, as a bare `f`. The module is this unit's
# own public bindings; the compiler names nothing here.
#
# Every operation is total: head, tail, nth and find answer an Option rather than
# aborting, take and drop saturate, and take/drop/range count from 0.
Lib = import "std/lib";
open Lib;

# length(xs) counts xs's elements; it is 0 for Nil.
pub length = fn[A : Type](xs : List(A)) : I64 {
  rec go = fn(xs : List(A), acc : I64) : I64 {
    match (xs) { Nil => acc, Cons(_, t) => go(t, acc + 1) }
  };
  go(xs, 0)
};

# reverse(xs) is xs back to front; it is Nil for Nil.
pub reverse = fn[A : Type](xs : List(A)) : List(A) {
  rec go = fn(xs : List(A), acc : List(A)) : List(A) {
    match (xs) { Nil => acc, Cons(h, t) => go(t, Cons(h, acc)) }
  };
  go(xs, Nil)
};

# append(xs, ys) is xs's elements followed by ys's; either side may be Nil.
pub append = fn[A : Type](xs : List(A), ys : List(A)) : List(A) {
  rec go = fn(xs : List(A)) : List(A) {
    match (xs) { Nil => ys, Cons(h, t) => Cons(h, go(t)) }
  };
  go(xs)
};

# map(f, xs) applies f to each element, in order; it is Nil for Nil.
pub map = fn[A : Type, B : Type](f : A -> B, xs : List(A)) : List(B) {
  rec go = fn(xs : List(A)) : List(B) {
    match (xs) { Nil => Nil, Cons(h, t) => Cons(f(h), go(t)) }
  };
  go(xs)
};

# filter(p, xs) keeps the elements p answers True for, in order; it is Nil for Nil.
pub filter = fn[A : Type](p : A -> Bool, xs : List(A)) : List(A) {
  rec go = fn(xs : List(A)) : List(A) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (p(h)) { True => Cons(h, go(t)), False => go(t) }
    }
  };
  go(xs)
};

# fold(f, z, xs) folds f over xs from the left, z first: f(…f(f(z, x0), x1)…); it is
# z for Nil.
pub fold = fn[A : Type, B : Type](f : B -> A -> B, z : B, xs : List(A)) : B {
  rec go = fn(xs : List(A), acc : B) : B {
    match (xs) { Nil => acc, Cons(h, t) => go(t, f(acc, h)) }
  };
  go(xs, z)
};

# concat(xss) is the elements of xss's lists in order; it is Nil for Nil and for a
# list of Nil. Each append copies the prefix it already has, so the total is
# quadratic in the result's length.
pub concat = fn[A : Type](xss : List(List(A))) : List(A) {
  fold(fn(acc : List(A), xs : List(A)) : List(A) { append(acc, xs) }, Nil, xss)
};

# find(p, xs) is the first element p answers True for; it is None for Nil or when p
# holds for none of them.
pub find = fn[A : Type](p : A -> Bool, xs : List(A)) : Option(A) {
  rec go = fn(xs : List(A)) : Option(A) {
    match (xs) {
      Nil => None,
      Cons(h, t) => match (p(h)) { True => Some(h), False => go(t) }
    }
  };
  go(xs)
};

# head(xs) is xs's first element; it is None for Nil.
pub head = fn[A : Type](xs : List(A)) : Option(A) {
  match (xs) { Nil => None, Cons(h, _) => Some(h) }
};

# tail(xs) is xs without its first element; it is None for Nil.
pub tail = fn[A : Type](xs : List(A)) : Option(List(A)) {
  match (xs) { Nil => None, Cons(_, t) => Some(t) }
};

# nth(i, xs) is xs's element at 0-based index i; it is None for i < 0, for i past the
# end and for Nil.
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

# head_or(d, xs) is xs's first element, or d for Nil.
pub head_or = fn[A : Type](d : A, xs : List(A)) : A {
  match (xs) { Nil => d, Cons(h, _) => h }
};

# nth_or(d, i, xs) is xs's element at 0-based index i, or d when nth finds none (a
# negative or past-the-end index, or Nil).
pub nth_or = fn[A : Type](d : A, i : I64, xs : List(A)) : A {
  match (nth(i, xs)) { Some(x) => x, None => d }
};

# take(n, xs) is xs's first n elements, or all of xs when n is past the end; it is Nil
# for n <= 0 and for Nil.
pub take = fn[A : Type](n : I64, xs : List(A)) : List(A) {
  rec go = fn(n : I64, xs : List(A)) : List(A) {
    match (i64_to_bool(le_i64(n, 0))) {
      True => Nil,
      False => match (xs) { Nil => Nil, Cons(h, t) => Cons(h, go(n - 1, t)) }
    }
  };
  go(n, xs)
};

# drop(n, xs) is xs without its first n elements, or Nil when n is past the end; it is
# xs for n <= 0 and Nil for Nil.
pub drop = fn[A : Type](n : I64, xs : List(A)) : List(A) {
  rec go = fn(n : I64, xs : List(A)) : List(A) {
    match (i64_to_bool(le_i64(n, 0))) {
      True => xs,
      False => match (xs) { Nil => Nil, Cons(_, t) => go(n - 1, t) }
    }
  };
  go(n, xs)
};

# zip(xs, ys) pairs xs's elements with ys's, in order, truncating at the shorter list;
# it is Nil when either side is Nil.
pub zip = fn[A : Type, B : Type](xs : List(A), ys : List(B)) : List(Tuple(2, A, B)) {
  rec go = fn(xs : List(A), ys : List(B)) : List(Tuple(2, A, B)) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (ys) { Nil => Nil, Cons(h2, t2) => Cons((h, h2), go(t, t2)) }
    }
  };
  go(xs, ys)
};

# zip_with(f, xs, ys) applies f to xs's and ys's elements pairwise, truncating at the
# shorter list; it is Nil when either side is Nil.
pub zip_with = fn[A : Type, B : Type, C : Type](f : A -> B -> C, xs : List(A), ys : List(B)) : List(C) {
  rec go = fn(xs : List(A), ys : List(B)) : List(C) {
    match (xs) {
      Nil => Nil,
      Cons(h, t) => match (ys) { Nil => Nil, Cons(h2, t2) => Cons(f(h, h2), go(t, t2)) }
    }
  };
  go(xs, ys)
};

# any(p, xs) says whether p answers True for any element; it is False for Nil.
pub any = fn[A : Type](p : A -> Bool, xs : List(A)) : Bool {
  rec go = fn(xs : List(A)) : Bool {
    match (xs) {
      Nil => False,
      Cons(h, t) => match (p(h)) { True => True, False => go(t) }
    }
  };
  go(xs)
};

# all(p, xs) says whether p answers True for every element; it is True for Nil.
pub all = fn[A : Type](p : A -> Bool, xs : List(A)) : Bool {
  rec go = fn(xs : List(A)) : Bool {
    match (xs) {
      Nil => True,
      Cons(h, t) => match (p(h)) { True => go(t), False => False }
    }
  };
  go(xs)
};

# range(n) is the 0-based indices below n, 0 first; it is Nil for n <= 0.
pub range = fn(n : I64) : List(I64) {
  rec go = fn(i : I64) : List(I64) {
    match (i64_to_bool(lt_i64(i, n))) { True => Cons(i, go(i + 1)), False => Nil }
  };
  go(0)
};
