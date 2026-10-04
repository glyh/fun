# The language's own surface: `if`, the operators and their fixity, the bare
# one-per-language helpers (not, and, or, min, max, abs), and Eq. What belongs here is
# what a program uses with no qualification, whatever type it is about.
# The bootstrap is re-exported, not just imported: a syntax form's rule resolves
# the names it writes against the unit that exports the form, and `if` writes True and
# False.
Core = import "std/bootstrap";
export Core;
open Core;

# not(b) inverts b.
pub not = fn(b) { match (b) { True => False, False => True } };

pub syntax if { if ($c) $(t : Block) else $(e : Block) => match ($c) { True => $t, False => $e } };

pub order disjunction;
pub order conjunction : stronger_than(disjunction);
pub order comparison : stronger_than(conjunction);
pub order additive : stronger_than(comparison);
pub order multiplicative : stronger_than(additive);
pub order negation : stronger_than(multiplicative);
pub order pipe : stronger_than(comparison) weaker_than(additive);
pub infix (&&) conjunction ($a, $b) { match ($a) { True => $b, False => False } };
pub infix (||) disjunction ($a, $b) { match ($a) { True => True, False => $b } };
pub infix (|>) pipe ($a, $b) { $b($a) };

pub (<) = fn(x, y) { i64_to_bool(lt_i64(x, y)) };
pub (>) = fn(x, y) { i64_to_bool(gt_i64(x, y)) };
pub (<=) = fn(x, y) { i64_to_bool(le_i64(x, y)) };
pub (>=) = fn(x, y) { i64_to_bool(ge_i64(x, y)) };

# The impls are named because a unit re-exporting this one may only take named
# public impls; the names are handles, not the evidence programs use.
pub trait Eq(a) = sig { eq : a -> a -> Bool };
pub impl i64_eq : Eq(I64) = module { fn eq(x, y) { i64_to_bool(eq_i64(x, y)) } };
pub impl bool_eq : Eq(Bool) = module { fn eq(x, y) { match (x) { True => y, False => not(y) } } };
pub impl char_eq : Eq(Char) = module { fn eq(x, y) { i64_to_bool(eq_char(x, y)) } };
pub impl unit_eq : Eq(Unit) = module { fn eq(x, y) { i64_to_bool(eq_unit(x, y)) } };
pub impl string_eq : Eq(String) = module { fn eq(x, y) { i64_to_bool(eq_string(x, y)) } };
pub (==) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) { Eq.eq(lhs, rhs) };
pub (!=) : [A : Eq] -> A -> A -> Bool = fn[A : Type](lhs, rhs) { not((==)[A](lhs, rhs)) };

pub infix (==) comparison;
pub infix (!=) comparison;
pub infix (<) comparison;
pub infix (>) comparison;
pub infix (<=) comparison;
pub infix (>=) comparison;
pub infix (+) additive;
pub infix (-) additive;
pub infix (*) multiplicative;
pub infix (/) multiplicative;
pub infix (%) multiplicative;
pub prefix (not) negation;

# and(a, b) is b when a is True, and False otherwise; `&&` is the same as a form.
pub and = fn(a : Bool, b : Bool) : Bool { match (a) { True => b, False => False } };

# or(a, b) is True when a is True, and b otherwise; `||` is the same as a form.
pub or = fn(a : Bool, b : Bool) : Bool { match (a) { True => True, False => b } };

# min(x, y) is the smaller of x and y (y when they are equal).
pub min = fn(x : I64, y : I64) : I64 {
  match (i64_to_bool(le_i64(x, y))) { True => x, False => y }
};

# max(x, y) is the larger of x and y (y when they are equal).
pub max = fn(x : I64, y : I64) : I64 {
  match (i64_to_bool(ge_i64(x, y))) { True => x, False => y }
};

# abs(x) is x's distance from 0; it overflows only for the least I64, which has no
# positive counterpart.
pub abs = fn(x : I64) : I64 {
  match (i64_to_bool(lt_i64(x, 0))) { True => 0 - x, False => x }
};
