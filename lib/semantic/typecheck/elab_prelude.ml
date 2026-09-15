open Elab_common

(* Every primitive's type comes from its one declaration ([Nbe_prim.declarations]). *)
let prims = Nbe_prim.declarations |> List.map (fun (d : Nbe_prim.declaration) -> (d.name, d.ty)) |> NameMap.of_list

let syntax_primitive_names = []

(* Stage 1: the types the rest of the prelude - and the [type] macro - are
   written with. Expanded with no elaborator, so it declares enums directly. *)
let stage1_source =
  {|
pub rec Bool = enum { False, True };
open Bool;
export Bool;
pub syntax if { if ($c) $(t : Block) else $(e : Block) => match ($c) { True => $t, False => $e } };
pub i64_to_bool = fn(n) { match (n) { 0 => False, _ => True } };
pub rec Option = fn(A : Type) { enum { Some(A), None } };
open Option;
export Option;
pub rec List = fn(A : Type) { enum { Nil, Cons(A, List(A)) } };
open List;
export List;
pub Syntax = module {
  pub rec Explicitness = enum { Explicit, Implicit };
  export Explicitness;
  pub rec Assoc = enum { Left, Right, NonAssoc };
  export Assoc;

  pub Span = struct {file: Option(String); start_byte: I64; end_byte: I64; start_line: Option(I64); start_col: Option(I64); end_line: Option(I64); end_col: Option(I64)};

  pub Id = struct {name: String; span: Option(Span); scope: Scopes};

  pub PathChoice = struct {opens: List(String); fallback: Option(String)};
  pub Path = struct {head: Id; members: List(String); head_choice: Option(PathChoice)};
  pub rec AtomVal = enum { I64Atom(I64), CharAtom(Char), StringAtom(String), UnitAtom, ScopesAtom(Scopes) };
  export AtomVal;
  pub rec AtomTy = enum { TyI64, TyUnit, TyChar, TyString, TyScopes, TyAbsurd };
  export AtomTy;
  pub rec Fixity = enum { PrefixFixity, InfixFixity };
  export Fixity;
  pub rec MacroAnn = enum { AnnExpr, AnnDecl };
  export MacroAnn;
  pub rec Expr = enum { RawVar(Option(Span), Id), RawAtom(Option(Span), AtomVal), RawSelf(Option(Span)), RawSelfType(Option(Span)), RawAp(Option(Span), Expr, Explicitness, Expr), RawLam(Option(Span), Param, Expr), RawLet(Option(Span), Id, Option(Expr), Expr, Expr, Bool), RawLetRecGroup(Option(Span), List(Id), List(Expr), Expr), RawAnnotated(Option(Span), Expr, Expr), RawProd(Option(Span), List(Expr)), RawProdTy(Option(Span), List(Expr)), RawArrow(Option(Span), Explicitness, Option(Id), Expr, Option(EffectRow), Expr), RawFieldAccess(Option(Span), Expr, String), RawProj(Option(Span), Expr, I64), RawRecordConstruct(Option(Span), Expr, List(Field)), RawStruct(Option(Span), List(Decl)), RawModule(Option(Span), List(Decl)), RawSig(Option(Span), List(Decl)), RawEnum(Option(Span), Option(String), List(Ctor)), RawImport(Option(Span), String, Scopes), RawOpen(Option(Span), Expr, Expr, String), RawOpenChoice(Option(Span), Id, List(String), Option(String)), RawTypeDef(Option(Span), TypeDecl, Expr), RawEffectDef(Option(Span), Id, List(Id), List(EffectOp), Expr), RawTraitDef(Option(Span), Id, List(Id), List(Field), Expr), RawImplDef(Option(Span), Option(Id), Path, List(Expr), List(Field), Expr), RawPerform(Option(Span), Path, Expr), RawResume(Option(Span), Expr), RawRefNew(Option(Span), Expr), RawRefGet(Option(Span), Expr), RawRefSet(Option(Span), Expr, Expr), RawMatch(Option(Span), Expr, List(Branch)), RawStx(Option(Span), Expr), RawQuote(Option(Span), Expr, List(QuoteHole)), RawQuoteDecls(Option(Span), List(Decl), List(QuoteHole)), RawMacroDef(Option(Span), Id, Expr, Expr, Option(MacroAnn), Option(Expr)), RawSyntaxDef(Option(Span), Id, Role, Expr), RawBlock(Option(Span), List(TokenTree)), RawInstantiate(Option(Span), Id, Rule, List(Capture), Option(String)), RawMacroCall(Option(Span), Expr, List(Captured)), RawOperatorUse(Option(Span), Id, Fixity, List(Expr), Option(Span), Option(Span), Option(String)) } and TokenTree = enum { Tok(Option(Span), TokenKind, Scopes), TokGroup(Option(Span), Delim, List(TokenTree)) } and TokenKind = enum { IdentTok(String), OperatorTok(String), IntTok(I64), CharTok(Char), StringTok(String), UnitTok, KeywordTok(String), PunctTok(String) } and Delim = enum { ParenDelim, BracketDelim, BraceDelim } and Role = enum { MkRole(Fixity, Option(Order), RoleMeaning, Option(Span), Option(String)) } and Order = enum { MkOrder(String, String, Assoc, Bool, List(Order), List(Order)) } and RoleMeaning = enum { ApplyValue, AssignRef, CallMacro, Rules(MacroAnn, List(Rule)), OrderGroup, TypeDeclaration } and Rule = enum { MkRule(List(RulePart), Replacement, Option(Span)) } and RulePart = enum { PartToken(TokenTree), PartGroup(Delim, List(RulePart), Option(Span)), PartHole(String, HoleKind, Option(Span)) } and HoleKind = enum { HoleExpr, HoleBlock, HoleId, HoleDecl, HoleOneDecl, HolePattern, HoleTokens } and Replacement = enum { ReplaceExpr(Expr), ReplaceDecls(List(Decl)) } and Capture = enum { MkCapture(String, Captured) } and Captured = enum { CapExpr(Expr), CapBlock(List(TokenTree)), CapId(TokenTree), CapPattern(Pattern), CapDecls(List(Decl)), CapDecl(Decl), CapTokens(List(TokenTree)) } and Field = enum { MkField(String, Expr) } and QuoteHole = enum { MkQuoteHole(String, Expr) } and Param = enum { MkParam(Id, Option(Expr), List(Path), Explicitness) } and EffectRow = enum { MkEffectRow(List(Expr), Option(Expr), Bool) } and EffectOp = enum { MkEffectOp(String, Expr, Expr) } and TypeDecl = enum { MkTypeDecl(Id, List(Id), List(Ctor)) } and Ctor = enum { MkCtor(Id, List(Expr)) } and Branch = enum { ValueBranch(Pattern, Expr), EffectBranch(Path, Pattern, Expr) } and Pattern = enum { RawPatWild(Option(Span)), RawPatBind(Option(Span), Id), RawPatCon(Option(Span), Path, List(Pattern)), RawPatAtom(Option(Span), AtomVal), RawPatProd(Option(Span), List(Pattern)), RawPatOr(Option(Span), Pattern, Pattern), RawPatRecord(Option(Span), Path, List(PatField), Bool), RawPatStructType(Option(Span), List(PatField), Bool), RawPatType(Option(Span), AtomTy) } and PatField = enum { MkPatField(String, Option(Pattern)) } and Decl = enum { DeclLet(Id, Expr, Bool, Bool), DeclRecGroup(List(Id), List(Expr), Bool), DeclMethod(Id, List(Param), Option(EffectRow), Expr, Bool), DeclType(List(TypeDecl), Bool), DeclEffect(Id, List(Id), List(EffectOp), Bool), DeclTrait(Id, List(Id), List(Field), Bool), DeclImpl(Option(Id), Path, List(Expr), List(Field), Bool), DeclMacro(Id, Expr, Bool, Option(MacroAnn), Option(Expr)), DeclMacroCall(Expr, List(Captured), Bool), DeclPatternSyn(Id, List(Id), Pattern, Bool), DeclField(String, Expr), DeclOpen(Expr, String), DeclExport(Expr, Option(List(String)), Bool), DeclHole(Id), DeclSyntax(Id, Role, Bool), DeclItems(List(TokenTree)), DeclInstantiate(Id, Rule, List(Capture), Option(String), Bool) };
  export Expr;
  export TokenTree;
  export TokenKind;
  export Delim;
  export Role;
  export Order;
  export RoleMeaning;
  export Rule;
  export RulePart;
  export HoleKind;
  export Replacement;
  export Capture;
  export Captured;
  export Field;
  export QuoteHole;
  export Param;
  export EffectRow;
  export EffectOp;
  export TypeDecl;
  export Ctor;
  export Branch;
  export Pattern;
  export PatField;
  export Decl;
  pub pattern Var(name) = Expr.RawVar(_, name);
  pub pattern Ap(f, a) = Expr.RawAp(_, f, _, a);
  pub pattern Lam(name, body) = Expr.RawLam(_, name, body);
  pub pattern Let(name, val, body) = Expr.RawLet(_, name, _, val, body, _);
  pub pattern Atom(val) = Expr.RawAtom(_, val);
  pub TypeExpr : Type = Type;
  pub pat_wild = Pattern.RawPatWild(None);
  pub pat_var = fn(id) { Pattern.RawPatBind(None, id) };
  pub pat_con = fn(name, args) { Pattern.RawPatCon(None, Path{head = name; members = Nil; head_choice = None}, args) };
  pub pat_atom = fn(val) { Pattern.RawPatAtom(None, val) };
  pub pat_prod = fn(pats) { Pattern.RawPatProd(None, pats) };
  pub pat_or = fn(l, r) { Pattern.RawPatOr(None, l, r) };
  pub pattern PatWild = Pattern.RawPatWild(_);
  pub pattern PatBind(name) = Pattern.RawPatBind(_, name);
  pub pattern PatCon(path, args) = Pattern.RawPatCon(_, path, args);
  pub pattern PatAtom(val) = Pattern.RawPatAtom(_, val);
  pub pattern PatProd(pats) = Pattern.RawPatProd(_, pats);
  pub pattern PatOr(l, r) = Pattern.RawPatOr(_, l, r);
  pub Decls = List(Decl);
  pub decl_let = fn(name, val, is_pub) { Decl.DeclLet(name, val, is_pub, False) };
  pub rec R = enum { RExpr(Type), RDecls, RPat };
  export R;
  pub atom_val = fn(val) { Expr.RawAtom(None, val) };
  pub ap = fn(f, a) { Expr.RawAp(None, f, Explicitness.Explicit, a) };
  pub i64 = fn(n) { atom_val(AtomVal.I64Atom(n)) };
  pub string = fn(s) { atom_val(AtomVal.StringAtom(s)) };
  pub char = fn(c) { atom_val(AtomVal.CharAtom(c)) };
  pub unit = fn(_) { atom_val(AtomVal.UnitAtom) };
  pub tokens = fn(b : Expr) { match (b) { Expr.RawBlock(_, ts) => ts, _ => panic[List(TokenTree)]("tokens: not a block") } };
  pub expand_block = fn(b : Expr) { expand_block[Expr](b) };
  pub expand_decls = fn(d : Decls) { expand_decls[Decls](d) };
  pub id_name = fn(stx) { match (stx) { Expr.RawVar(_, id) => id.name, _ => panic[String]("expected identifier") } };
  pub id_eq = fn(a, b) { match (a) { Expr.RawVar(_, ida) => match (b) { Expr.RawVar(_, idb) => i64_to_bool(eq_string(ida.name, idb.name)), _ => panic[Bool]("expected identifier") }, _ => panic[Bool]("expected identifier") } }

}
|}

(* Stage 2: expanded and elaborated with stage 1 as [import "std"], so it can
   define procedural macros - [type] among them. It re-exports stage 1. *)
let stage2_source =
  {|
Core = import "std";
export Core;
open Core;
pub not = fn(b) { match (b) { True => False, False => True } };
pub order disjunction;
pub order conjunction : stronger_than(disjunction);
pub order comparison : stronger_than(conjunction);
pub order additive : stronger_than(comparison);
pub order multiplicative : stronger_than(additive);
pub order negation : stronger_than(multiplicative);
pub order arrow : weaker_than(disjunction) assoc(right);
pub infix (~>) arrow ($a, $b) { $a -> $b can _ };
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
|}

(* Stage 1's roles ([if]): what stage 2 sees through its [import "std"]. *)
let stage1_syntax_exports = lazy (Parse_expand.syntax_exports stage1_source)

let parsed_stage1 = lazy (Parse_expand.parse_module stage1_source)
