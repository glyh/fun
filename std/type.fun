# The `type` macro and its token helpers. The compiler does not read this unit;
# it is the library the `std` unit re-exports so a program can write `type`.
# The library is re-exported, not just imported: the `type` form's rule resolves
# the names it writes (TokenTree, Decl) against the unit that exports the form.
Ops = import "std/lib";
export Ops;
open Ops;
Lists = import "std/list";
open Lists;

tok_is = fn(t : Syntax.TokenTree, k : Syntax.TokenKind) : Bool {
  match (t) {
    Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(s), _) =>
      match (k) { Syntax.TokenKind.IdentTok(w) => i64_to_bool(eq_string(s, w)), _ => False },
    Syntax.TokenTree.Tok(_, Syntax.TokenKind.PunctTok(s), _) =>
      match (k) { Syntax.TokenKind.PunctTok(w) => i64_to_bool(eq_string(s, w)), _ => False },
    _ => False
  }
};

tok_split = fn(l : List(Syntax.TokenTree), sep : Syntax.TokenKind)
          : List(List(Syntax.TokenTree)) {
  rec go = fn(l : List(Syntax.TokenTree), cur : List(Syntax.TokenTree),
              acc : List(List(Syntax.TokenTree))) {
    match (l) {
      Nil => rev(Cons(rev(cur), acc)),
      Cons(h, t) =>
        if (tok_is(h, sep)) { go(t, Nil, Cons(rev(cur), acc)) }
        else { go(t, Cons(h, cur), acc) }
    }
  };
  go(l, Nil, Nil)
};

first_group = fn(g : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) {
  match (g) { Cons(h, _) => h, Nil => Nil }
};
second_group = fn(g : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) {
  match (g) { Cons(_, Cons(r, _)) => r, _ => Nil }
};

# The type macro's failure texts, one helper per message. A malformed declaration is a
# language error, so it panics; the message cannot append the offending token's own
# spelling because the language has no string concatenation, and the source position it
# lacks is the map's existing "elaborator errors carry no source location" fog.
name_expected = fn[T : Type]() : T { panic[T]("type: expected a name") };
declaration_expected = fn[T : Type]() : T { panic[T]("type: expected a declaration") };
type_expected = fn[T : Type]() : T { panic[T]("type: Type") };
record_is_a_value = fn[T : Type]() : T {
  panic[T]("a record type is a value: write X = struct { … } or rec X = struct { … }")
};

tok_scope = fn(t : Syntax.TokenTree) : Scopes {
  match (t) { Syntax.TokenTree.Tok(_, _, sc) => sc, _ => name_expected[Scopes]() }
};
type_name = fn(member : List(Syntax.TokenTree)) : Syntax.TokenTree {
  match (first_group(tok_split(member, Syntax.TokenKind.PunctTok("=")))) {
    Cons(n, _) => n,
    Nil => name_expected[Syntax.TokenTree]()
  }
};

rec type_params = fn(l : List(Syntax.TokenTree)) : List(Syntax.TokenTree) {
  match (l) {
    Nil => Nil,
    Cons(h, t) => match (h) {
      Syntax.TokenTree.TokGroup(_, _, items) => append(type_params(items), type_params(t)),
      Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(_), _) => Cons(h, type_params(t)),
      _ => type_params(t)
    }
  }
};

rec param_decls = fn(ps : List(Syntax.TokenTree), sc : Scopes,
                     ty : Syntax.TokenTree) : List(Syntax.TokenTree) {
  match (ps) {
    Nil => Nil,
    Cons(p, rest) => {
      colon = Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(":"), sc);
      decl = Cons(p, Cons(colon, Cons(ty, Nil)));
      match (rest) {
        Nil => decl,
        _ => {
          comma = Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(","), sc);
          append(decl, Cons(comma, param_decls(rest, sc, ty)))
        }
      }
    }
  }
};

has_comma = fn(items : List(Syntax.TokenTree)) : Bool {
  fold(fn(acc, h) {
         if (tok_is(h, Syntax.TokenKind.PunctTok(","))) { True } else { acc }
       }, False, items)
};

ctor_tokens = fn(alt : List(Syntax.TokenTree)) : List(Syntax.TokenTree) {
  match (alt) {
    Nil => Nil,
    Cons(name, payload) => {
      wrapped = Cons(name,
                     Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.ParenDelim, payload),
                          Nil));
      match (payload) {
        Nil => alt,
        Cons(only, Nil) => match (only) {
          Syntax.TokenTree.TokGroup(_, Syntax.Delim.ParenDelim, items) =>
            if (has_comma(items)) { alt } else { wrapped },
          _ => wrapped
        },
        _ => wrapped
      }
    }
  }
};
ctor_alts = fn(gs : List(List(Syntax.TokenTree))) : List(List(Syntax.TokenTree)) {
  map(ctor_tokens, gs)
};

rec join_commas = fn(gs : List(List(Syntax.TokenTree)), sc : Scopes)
                : List(Syntax.TokenTree) {
  match (gs) {
    Nil => Nil,
    Cons(g, rest) => {
      more = join_commas(rest, sc);
      match (g) {
        Nil => more,
        _ => match (more) {
          Nil => g,
          _ => append(g, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(","), sc),
                              more))
        }
      }
    }
  }
};

type_member = fn(member : List(Syntax.TokenTree), ty : Syntax.TokenTree)
            : List(Syntax.TokenTree) {
  sides = tok_split(member, Syntax.TokenKind.PunctTok("="));
  name = type_name(member);
  sc = tok_scope(name);
  params = match (first_group(sides)) { Cons(_, rest) => type_params(rest), Nil => Nil };
  alts = ctor_alts(tok_split(second_group(sides), Syntax.TokenKind.PunctTok("|")));
  enum_body = Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("enum"), sc),
                   Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.BraceDelim,
                                                  join_commas(alts, sc)),
                        Nil));
  body = match (params) {
    Nil => enum_body,
    _ => Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("fn"), sc),
              Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.ParenDelim,
                                             param_decls(params, sc, ty)),
                   Cons(Syntax.TokenTree.TokGroup(None, Syntax.Delim.BraceDelim, enum_body),
                        Nil)))
  };
  Cons(name, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok("="), sc), body))
};

rec type_members = fn(members : List(List(Syntax.TokenTree)),
                      ty : Syntax.TokenTree) : List(Syntax.TokenTree) {
  match (members) {
    Nil => Nil,
    Cons(m, rest) => match (rest) {
      Nil => type_member(m, ty),
      _ => append(type_member(m, ty),
                  Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.IdentTok("and"),
                                            tok_scope(type_name(m))),
                       type_members(rest, ty)))
    }
  }
};

rec type_opens = fn(members : List(List(Syntax.TokenTree))) : List(Syntax.TokenTree) {
  match (members) {
    Nil => Nil,
    Cons(m, rest) =>
      Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.PunctTok(";"),
                                tok_scope(type_name(m))),
           Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("open"),
                                     tok_scope(type_name(m))),
                Cons(type_name(m), type_opens(rest))))
  }
};

export_of = fn(m : List(Syntax.TokenTree)) : Syntax.Decl {
  match (type_name(m)) {
    Syntax.TokenTree.Tok(_, Syntax.TokenKind.IdentTok(n), sc) =>
      Syntax.Decl.DeclExport(
        Syntax.Expr.RawVar(None, Syntax.Id{name = n; span = None; scope = sc}), None, False),
    _ => name_expected[Syntax.Decl]()
  }
};
type_exports = fn(members : List(List(Syntax.TokenTree))) : List(Syntax.Decl) {
  map(export_of, members)
};

record_rhs = fn(member : List(Syntax.TokenTree)) : Bool {
  match (second_group(tok_split(member, Syntax.TokenKind.PunctTok("=")))) {
    Cons(Syntax.TokenTree.Tok(_, Syntax.TokenKind.KeywordTok(k), _), _) =>
      i64_to_bool(eq_string(k, "struct")),
    _ => False
  }
};

rec check_records = fn(members : List(List(Syntax.TokenTree))) : Unit {
  match (members) {
    Nil => (),
    Cons(m, rest) =>
      if (record_rhs(m)) { record_is_a_value[Unit]() }
      else { check_records(rest) }
  }
};

pub macro type_decls(ts : List(TokenTree)) : List(Decl) {
  members = tok_split(ts, Syntax.TokenKind.IdentTok("and"));
  _ = check_records(members);
  type_scope = match (quote(Type)) {
    Syntax.Expr.RawVar(_, i) => i.scope,
    _ => type_expected[Scopes]()
  };
  ty = Syntax.TokenTree.Tok(None, Syntax.TokenKind.IdentTok("Type"), type_scope);
  first = match (members) {
    Cons(m, _) => type_name(m),
    Nil => declaration_expected[Syntax.TokenTree]()
  };
  decls = Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.KeywordTok("rec"), tok_scope(first)),
               type_members(members, ty));
  opens = match (type_opens(members)) { Cons(_, rest) => rest, Nil => Nil };
  Cons(Syntax.Decl.DeclItems(decls),
       append(type_exports(members), Cons(Syntax.Decl.DeclItems(opens), Nil)))
};
pub syntax type : Decl { type $(r : List(TokenTree)) => { type_decls($r) } };
