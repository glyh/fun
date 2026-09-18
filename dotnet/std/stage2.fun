Core = import "std/stage1";
export Core;
open Core;
pub not = fn(b) { match (b) { True => False, False => True } };
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
pub trait Eq(A) = sig { eq : A -> A -> Bool };
pub impl Eq(I64) = module { fn eq(x, y) { i64_to_bool(eq_i64(x, y)) } };
pub impl Eq(Bool) = module { fn eq(x, y) { match (x) { True => y, False => not(y) } } };
pub impl Eq(Char) = module { fn eq(x, y) { i64_to_bool(eq_char(x, y)) } };
pub impl Eq(Unit) = module { fn eq(x, y) { i64_to_bool(eq_unit(x, y)) } };
pub impl Eq(String) = module { fn eq(x, y) { i64_to_bool(eq_string(x, y)) } };
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

tok_rev = fn(l : List(Syntax.TokenTree)) : List(Syntax.TokenTree) {
  rec go = fn(l : List(Syntax.TokenTree), acc : List(Syntax.TokenTree)) : List(Syntax.TokenTree) { match (l) { Nil => acc, Cons(h, t) => go(t, Cons(h, acc)) } };
  go(l, Nil)
};
rec tok_append = fn(a : List(Syntax.TokenTree), b : List(Syntax.TokenTree)) : List(Syntax.TokenTree) { match (a) { Nil => b, Cons(h, t) => Cons(h, tok_append(t, b)) } };
groups_rev = fn(l : List(List(Syntax.TokenTree))) : List(List(Syntax.TokenTree)) {
  rec go = fn(l : List(List(Syntax.TokenTree)), acc : List(List(Syntax.TokenTree))) : List(List(Syntax.TokenTree)) { match (l) { Nil => acc, Cons(h, t) => go(t, Cons(h, acc)) } };
  go(l, Nil)
};
tok_is = fn(t : Syntax.TokenTree, k : Syntax.TokenKind) : Bool {
  match (t) {
    Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(s), _) => match (k) { Syntax.TokenKind.IdentTok(w) => i64_to_bool(eq_string(s, w)), _ => False },
    Syntax.TokenTree.Tok(_, Syntax.TokenKind.PunctTok(s), _) => match (k) { Syntax.TokenKind.PunctTok(w) => i64_to_bool(eq_string(s, w)), _ => False },
    _ => False
  }
};
tok_split = fn(l : List(Syntax.TokenTree), sep : Syntax.TokenKind) : List(List(Syntax.TokenTree)) {
  rec go = fn(l : List(Syntax.TokenTree), cur : List(Syntax.TokenTree), acc : List(List(Syntax.TokenTree))) : List(List(Syntax.TokenTree)) {
    match (l) {
      Nil => groups_rev(Cons(tok_rev(cur), acc)),
      Cons(h, t) => if (tok_is(h, sep)) { go(t, Nil, Cons(tok_rev(cur), acc)) } else { go(t, Cons(h, cur), acc) }
    }
  };
  go(l, Nil, Nil)
};
first_group = fn(g : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) { match (g) { Cons(h, _) => h, Nil => Nil } };
second_group = fn(g : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) { match (g) { Cons(_, Cons(r, _)) => r, _ => Nil } };
tok_scope = fn(t : Syntax.TokenTree) : Scopes { match (t) { Syntax.TokenTree.Tok(_, _, sc) => sc, _ => panic[Scopes]("type: expected a name") } };
type_name = fn(member : List(Syntax.TokenTree)) : Syntax.TokenTree { match (first_group(tok_split(member, Syntax.TokenKind.PunctTok("=")))) { Cons(n, _) => n, Nil => panic[Syntax.TokenTree]("type: expected a name") } };
rec type_params = fn(l : List(Syntax.TokenTree)) : List(Syntax.TokenTree) {
  match (l) {
    Nil => Nil,
    Cons(h, t) => match (h) {
      Syntax.TokenTree.TokGroup(_, _, items) => tok_append(type_params(items), type_params(t)),
      Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(_), _) => Cons(h, type_params(t)),
      _ => type_params(t)
    }
  }
};
rec param_decls = fn(ps : List(Syntax.TokenTree), sc : Scopes, ty : Syntax.TokenTree) : List(Syntax.TokenTree) {
  match (ps) {
    Nil => Nil,
    Cons(p, rest) => {
      decl = Cons(p, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(":"), sc), Cons(ty, Nil)));
      match (rest) { Nil => decl, _ => tok_append(decl, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(","), sc), param_decls(rest, sc, ty))) }
    }
  }
};
has_comma = fn(items : List(Syntax.TokenTree)) : Bool {
  rec go = fn(l : List(Syntax.TokenTree)) : Bool { match (l) { Nil => False, Cons(h, t) => if (tok_is(h, Syntax.TokenKind.PunctTok(","))) { True } else { go(t) } } };
  go(items)
};
ctor_tokens = fn(alt : List(Syntax.TokenTree)) : List(Syntax.TokenTree) {
  match (alt) {
    Nil => Nil,
    Cons(name, payload) => {
      wrapped = Cons(name, Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.ParenDelim, payload), Nil));
      match (payload) {
        Nil => alt,
        Cons(only, Nil) => match (only) {
          Syntax.TokenTree.TokGroup(_, Syntax.Delim.ParenDelim, items) => if (has_comma(items)) { alt } else { wrapped },
          _ => wrapped
        },
        _ => wrapped
      }
    }
  }
};
rec ctor_alts = fn(gs : List(List(Syntax.TokenTree))) : List(List(Syntax.TokenTree)) { match (gs) { Nil => Nil, Cons(g, rest) => Cons(ctor_tokens(g), ctor_alts(rest)) } };
rec join_commas = fn(gs : List(List(Syntax.TokenTree)), sc : Scopes) : List(Syntax.TokenTree) {
  match (gs) {
    Nil => Nil,
    Cons(g, rest) => {
      more = join_commas(rest, sc);
      match (g) {
        Nil => more,
        _ => match (more) { Nil => g, _ => tok_append(g, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(","), sc), more)) }
      }
    }
  }
};
type_member = fn(member : List(Syntax.TokenTree), ty : Syntax.TokenTree) : List(Syntax.TokenTree) {
  sides = tok_split(member, Syntax.TokenKind.PunctTok("="));
  name = type_name(member);
  sc = tok_scope(name);
  params = match (first_group(sides)) { Cons(_, rest) => type_params(rest), Nil => Nil };
  enum_body = Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("enum"), sc),
                   Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.BraceDelim, join_commas(ctor_alts(tok_split(second_group(sides), Syntax.TokenKind.PunctTok("|"))), sc)), Nil));
  body = match (params) {
    Nil => enum_body,
    _ => Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("fn"), sc),
              Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.ParenDelim, param_decls(params, sc, ty)),
                   Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.BraceDelim, enum_body), Nil)))
  };
  Cons(name, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok("="), sc), body))
};
rec type_members = fn(members : List(List(Syntax.TokenTree)), ty : Syntax.TokenTree) : List(Syntax.TokenTree) {
  match (members) {
    Nil => Nil,
    Cons(m, rest) => match (rest) {
      Nil => type_member(m, ty),
      _ => tok_append(type_member(m, ty), Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.IdentTok("and"), tok_scope(type_name(m))), type_members(rest, ty)))
    }
  }
};
rec type_opens = fn(members : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) {
  match (members) {
    Nil => Nil,
    Cons(m, rest) => Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(";"), tok_scope(type_name(m))),
                          Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("open"), tok_scope(type_name(m))),
                               Cons(type_name(m), type_opens(rest))))
  }
};
rec type_exports = fn(members : List(List(Syntax.TokenTree))) : List(Syntax.Decl) {
  match (members) {
    Nil => Nil,
    Cons(m, rest) => match (type_name(m)) {
      Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(n), sc) =>
        Cons(Syntax.Decl.DeclExport(Syntax.Expr.RawVar(None, Syntax.Id{name = n; span = None; scope = sc}), None, False), type_exports(rest)),
      _ => panic[List(Syntax.Decl)]("type: expected a name")
    }
  }
};
rec append_decls = fn(a : List(Syntax.Decl), b : List(Syntax.Decl)) : List(Syntax.Decl) { match (a) { Nil => b, Cons(h, t) => Cons(h, append_decls(t, b)) } };
record_rhs = fn(member : List(Syntax.TokenTree)) : Bool {
  match (second_group(tok_split(member, Syntax.TokenKind.PunctTok("=")))) {
    Cons(Syntax.TokenTree.Tok(_, Syntax.TokenKind.KeywordTok(k), _), _) => i64_to_bool(eq_string(k, "struct")),
    _ => False
  }
};
rec check_records = fn(members : List(List(Syntax.TokenTree))) : Unit {
  match (members) {
    Nil => (),
    Cons(m, rest) => if (record_rhs(m)) { panic[Unit]("a record type is a value: write X = struct { … } or rec X = struct { … }") } else { check_records(rest) }
  }
};
pub macro type_decls(ts : List(TokenTree)) : List(Decl) {
  members = tok_split(ts, Syntax.TokenKind.IdentTok("and"));
  _ = check_records(members);
  type_scope = match (quote(Type)) { Syntax.Expr.RawVar(_, i) => i.scope, _ => panic[Scopes]("type: Type") };
  ty = Syntax.TokenTree.Tok(None, Syntax.TokenKind.IdentTok("Type"), type_scope);
  first = match (members) { Cons(m, _) => type_name(m), Nil => panic[Syntax.TokenTree]("type: expected a declaration") };
  decls = Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("rec"), tok_scope(first)), type_members(members, ty));
  opens = match (type_opens(members)) { Cons(_, rest) => rest, Nil => Nil };
  Cons(Syntax.Decl.DeclItems(decls), append_decls(type_exports(members), Cons(Syntax.Decl.DeclItems(opens), Nil)))
};
pub syntax type : Decl { type $(r : List(TokenTree)) => { type_decls($r) } };
