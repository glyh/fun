# a macro's output holds a reflected unit token, in a block
{ macro m(x : Id) : Expr(_) {
    Syntax.RawBlock(None, Cons(Syntax.TokenTree.Tok(None, Syntax.TokenKind.UnitTok, x.scope), Nil))
  };
  m(marker) == () }
