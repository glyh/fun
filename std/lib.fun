# The language's own surface: `if`, the operators and their fixity, and Eq.
# The bootstrap is re-exported, not just imported: a syntax form's rule resolves
# the names it writes against the unit that exports the form, and `if` writes
# True and False.
Core = import "std/bootstrap";
export Core;
open Core;

pub not = fn(b) { match (b) { True => False, False => True } };
pub syntax if { if ($c) $(t : Block) else $(e : Block) => match ($c) { True => $t, False => $e } };

pub order disjunction;
pub order conjunction : stronger_than(disjunction);
pub order comparison : stronger_than(conjunction);
pub order additive : stronger_than(comparison);
pub order multiplicative : stronger_than(additive);
pub order negation : stronger_than(multiplicative);
pub infix (&&) conjunction ($a, $b) { match ($a) { True => $b, False => False } };
pub infix (||) disjunction ($a, $b) { match ($a) { True => True, False => $b } };

pub (<) = fn(x, y) { i64_to_bool(lt_i64(x, y)) };
pub (>) = fn(x, y) { i64_to_bool(gt_i64(x, y)) };
pub (<=) = fn(x, y) { i64_to_bool(le_i64(x, y)) };
pub (>=) = fn(x, y) { i64_to_bool(ge_i64(x, y)) };

# The impls are named because a unit re-exporting this one may only take named
# public impls; the names are handles, not the evidence programs use.
pub trait Eq(A) = sig { eq : A -> A -> Bool };
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
