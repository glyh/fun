# an operator use spliced beside the infix decl that binds it resolves to that decl, not to whatever the use site has (Syntax.Map.cs:168)
{
  M = module {
    macro ops() : List(Decl) {
      quote { infix (+++) (x, y) { x }; pub y = 1 +++ 2 }
    };
    ops()
  };
  M.y
}
