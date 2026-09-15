open Elab_common

(* Every primitive's type comes from its one declaration ([Nbe_prim.declarations]). *)
let prims = Nbe_prim.declarations |> List.map (fun (d : Nbe_prim.declaration) -> (d.name, d.ty)) |> NameMap.of_list

let syntax_primitive_names = []

let stdlib_source =
  {|
pub type Bool = False | True;
pub syntax if { if ($c) $(t : Block) else $(e : Block) => match ($c) { True => $t, False => $e } };
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
pub i64_to_bool = fn(n) { match (n) { 0 => False, _ => True } };
pub not = fn(b) { match (b) { True => False, False => True } };
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
pub type Option(A) = Some(A) | None;
pub type List(A) = Nil | Cons(A, List(A));
pub Syntax = module {
  pub type Explicitness = Explicit | Implicit;
  pub type Assoc = Left | Right | NonAssoc;

  pub Span = struct {file: Option(String); start_byte: I64; end_byte: I64; start_line: Option(I64); start_col: Option(I64); end_line: Option(I64); end_col: Option(I64)};

  pub Id = struct {name: String; span: Option(Span); scope: Scopes};

  pub PathChoice = struct {opens: List(String); fallback: Option(String)};
  pub Path = struct {head: Id; members: List(String); head_choice: Option(PathChoice)};
  pub type AtomVal = I64Atom(I64) | CharAtom(Char) | StringAtom(String) | UnitAtom | ScopesAtom(Scopes);
  pub type AtomTy = TyI64 | TyUnit | TyChar | TyString | TyScopes | TyAbsurd;
  pub type Fixity = PrefixFixity | InfixFixity;
  pub type MacroAnn = AnnExpr | AnnDecl;
  pub type Expr =
    | RawVar(Option(Span), Id)
    | RawAtom(Option(Span), AtomVal)
    | RawSelf(Option(Span))
    | RawSelfType(Option(Span))
    | RawAp(Option(Span), Expr, Explicitness, Expr)
    | RawLam(Option(Span), Param, Expr)
    | RawLet(Option(Span), Id, Option(Expr), Expr, Expr, Bool)
    | RawLetRecGroup(Option(Span), List(Id), List(Expr), Expr)
    | RawAnnotated(Option(Span), Expr, Expr)
    | RawProd(Option(Span), List(Expr))
    | RawProdTy(Option(Span), List(Expr))
    | RawArrow(Option(Span), Explicitness, Option(Id), Expr, Option(EffectRow), Expr)
    | RawFieldAccess(Option(Span), Expr, String)
    | RawProj(Option(Span), Expr, I64)
    | RawRecordConstruct(Option(Span), Expr, List(Field))
    | RawStruct(Option(Span), List(Decl))
    | RawModule(Option(Span), List(Decl))
    | RawSig(Option(Span), List(Decl))
    | RawImport(Option(Span), String, Scopes)
    | RawOpen(Option(Span), Expr, Expr, String)
    | RawOpenChoice(Option(Span), Id, List(String), Option(String))
    | RawTypeDef(Option(Span), TypeDecl, Expr)
    | RawEffectDef(Option(Span), Id, List(Id), List(EffectOp), Expr)
    | RawTraitDef(Option(Span), Id, List(Id), List(Field), Expr)
    | RawImplDef(Option(Span), Option(Id), Path, List(Expr), List(Field), Expr)
    | RawPerform(Option(Span), Path, Expr)
    | RawResume(Option(Span), Expr)
    | RawRefNew(Option(Span), Expr)
    | RawRefGet(Option(Span), Expr)
    | RawRefSet(Option(Span), Expr, Expr)
    | RawMatch(Option(Span), Expr, List(Branch))
    | RawStx(Option(Span), Expr)
    | RawQuote(Option(Span), Expr, List(QuoteHole))
    | RawQuoteDecls(Option(Span), List(Decl), List(QuoteHole))
    | RawMacroDef(Option(Span), Id, Expr, Expr, Option(MacroAnn), Option(Expr))
    | RawSyntaxDef(Option(Span), Id, Role, Expr)
    | RawBlock(Option(Span), List(TokenTree))
    | RawInstantiate(Option(Span), Id, Rule, List(Capture), Option(String))
    | RawMacroCall(Option(Span), Expr, List(Captured))
    | RawOperatorUse(Option(Span), Id, Fixity, List(Expr), Option(Span), Option(Span), Option(String))
  and TokenTree = Tok(Option(Span), TokenKind, Scopes) | TokGroup(Option(Span), Delim, List(TokenTree))
  and TokenKind = IdentTok(String) | OperatorTok(String) | IntTok(I64) | CharTok(Char) | StringTok(String) | UnitTok | KeywordTok(String) | PunctTok(String)
  and Delim = ParenDelim | BracketDelim | BraceDelim
  and Role = MkRole(Fixity, Option(Order), RoleMeaning, Option(Span), Option(String))
  and Order = MkOrder(String, String, Assoc, Bool, List(Order), List(Order))
  and RoleMeaning = ApplyValue | AssignRef | CallMacro | Rules(MacroAnn, List(Rule)) | OrderGroup | TypeDeclaration
  and Rule = MkRule(List(RulePart), Replacement, Option(Span))
  and RulePart = PartToken(TokenTree) | PartGroup(Delim, List(RulePart), Option(Span)) | PartHole(String, HoleKind, Option(Span))
  and HoleKind = HoleExpr | HoleBlock | HoleId | HoleDecl | HoleOneDecl | HolePattern | HoleTokens
  and Replacement = ReplaceExpr(Expr) | ReplaceDecls(List(Decl))
  and Capture = MkCapture(String, Captured)
  and Captured = CapExpr(Expr) | CapBlock(List(TokenTree)) | CapId(TokenTree) | CapPattern(Pattern) | CapDecls(List(Decl)) | CapDecl(Decl) | CapTokens(List(TokenTree))
  and Field = MkField(String, Expr)
  and QuoteHole = MkQuoteHole(String, Expr)
  and Param = MkParam(Id, Option(Expr), List(Path), Explicitness)
  and EffectRow = MkEffectRow(List(Expr), Option(Expr), Bool)
  and EffectOp = MkEffectOp(String, Expr, Expr)
  and TypeDecl = MkTypeDecl(Id, List(Id), List(Ctor))
  and Ctor = MkCtor(Id, List(Expr))
  and Branch = ValueBranch(Pattern, Expr) | EffectBranch(Path, Pattern, Expr)
  and Pattern =
    | RawPatWild(Option(Span))
    | RawPatBind(Option(Span), Id)
    | RawPatCon(Option(Span), Path, List(Pattern))
    | RawPatAtom(Option(Span), AtomVal)
    | RawPatProd(Option(Span), List(Pattern))
    | RawPatOr(Option(Span), Pattern, Pattern)
    | RawPatRecord(Option(Span), Path, List(PatField), Bool)
    | RawPatStructType(Option(Span), List(PatField), Bool)
    | RawPatType(Option(Span), AtomTy)
  and PatField = MkPatField(String, Option(Pattern))
  and Decl =
    | DeclLet(Id, Expr, Bool, Bool)
    | DeclRecGroup(List(Id), List(Expr), Bool)
    | DeclMethod(Id, List(Param), Option(EffectRow), Expr, Bool)
    | DeclType(List(TypeDecl), Bool)
    | DeclEffect(Id, List(Id), List(EffectOp), Bool)
    | DeclTrait(Id, List(Id), List(Field), Bool)
    | DeclImpl(Option(Id), Path, List(Expr), List(Field), Bool)
    | DeclMacro(Id, Expr, Bool, Option(MacroAnn), Option(Expr))
    | DeclMacroCall(Expr, List(Captured), Bool)
    | DeclPatternSyn(Id, List(Id), Pattern, Bool)
    | DeclField(String, Expr)
    | DeclOpen(Expr, String)
    | DeclHole(Id)
    | DeclSyntax(Id, Role, Bool)
    | DeclItems(List(TokenTree))
    | DeclInstantiate(Id, Rule, List(Capture), Option(String), Bool);
  pub pattern Var(name) = RawVar(_, name);
  pub pattern Ap(f, a) = RawAp(_, f, _, a);
  pub pattern Lam(name, body) = RawLam(_, name, body);
  pub pattern Let(name, val, body) = RawLet(_, name, _, val, body, _);
  pub pattern Atom(val) = RawAtom(_, val);
  pub TypeExpr : Type = Type;
  pub pat_wild = RawPatWild(None);
  pub pat_var = fn(id) { RawPatBind(None, id) };
  pub pat_con = fn(name, args) { RawPatCon(None, Path{head = name; members = Nil; head_choice = None}, args) };
  pub pat_atom = fn(val) { RawPatAtom(None, val) };
  pub pat_prod = fn(pats) { RawPatProd(None, pats) };
  pub pat_or = fn(l, r) { RawPatOr(None, l, r) };
  pub pattern PatWild = RawPatWild(_);
  pub pattern PatBind(name) = RawPatBind(_, name);
  pub pattern PatCon(path, args) = RawPatCon(_, path, args);
  pub pattern PatAtom(val) = RawPatAtom(_, val);
  pub pattern PatProd(pats) = RawPatProd(_, pats);
  pub pattern PatOr(l, r) = RawPatOr(_, l, r);
  pub Decls = List(Decl);
  pub decl_let = fn(name, val, is_pub) { DeclLet(name, val, is_pub, False) };
  pub type R = RExpr(Type) | RDecls | RPat;
  pub atom_val = fn(val) { RawAtom(None, val) };
  pub ap = fn(f, a) { RawAp(None, f, Explicit, a) };
  pub i64 = fn(n) { atom_val(I64Atom(n)) };
  pub string = fn(s) { atom_val(StringAtom(s)) };
  pub char = fn(c) { atom_val(CharAtom(c)) };
  pub unit = fn(_) { atom_val(UnitAtom) };
  pub tokens = fn(b : Expr) { match (b) { RawBlock(_, ts) => ts, _ => panic[List(TokenTree)]("tokens: not a block") } };
  pub expand_block = fn(b : Expr) { expand_block[Expr](b) };
  pub expand_decls = fn(d : Decls) { expand_decls[Decls](d) };
  pub id_name = fn(stx) { match (stx) { RawVar(_, id) => id.name, _ => panic[String]("expected identifier") } };
  pub id_eq = fn(a, b) { match (a) { RawVar(_, ida) => match (b) { RawVar(_, idb) => i64_to_bool(eq_string(ida.name, idb.name)), _ => panic[Bool]("expected identifier") }, _ => panic[Bool]("expected identifier") } }

}
|}

(* The stdlib's public syntax exports (templates like [if]; the arithmetic and
   comparison operators). Under the strict phase rule these are delivered only
   where [std] is opened: the parser resolves them through [load_syntax] on the
   reserved [import "std"] path (see [std_load_syntax] and [Core_loader]), rather
   than through the old [builtin_syntax_hook] inversion ref. Collecting the
   exports uses a non-seeded env, so there is no recursion even though this same
   source is later parsed for elaboration. *)
let stdlib_syntax_exports = lazy (Parse_expand.syntax_exports stdlib_source)

(* A [load_syntax] resolver for parses that have no loader (expression eval,
   the REPL's non-file input): it answers the reserved [import "std"] path with
   the prelude's exports and knows no other module. Compose with the loader's
   own [load_syntax_exports] when files must also resolve. *)
let std_load_syntax path =
  if String.equal path Compiler_names.Module_name.std_import_path
  then Lazy.force stdlib_syntax_exports else []

(* The prelude body itself uses only prim calls and [match] (no infix operators),
   so it parses correctly without any [builtin_syntax] seed. *)
let parsed_stdlib = lazy (Parse_expand.parse_module stdlib_source)
