# a non-trailing Decl hole takes one { … } group: the group scopes over the body
{
  syntax with_decls {
    with_decls $(ds : List(Decl)) in $(body : Expr) => { r = module { $ds; pub value = $body }; r.value }
  };
  with_decls { x = 1; y = 2 } in x + y
}
