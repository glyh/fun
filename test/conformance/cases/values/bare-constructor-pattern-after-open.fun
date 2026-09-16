# after open, a bare pattern head resolves to the opened constructor
{ Color = enum { Red, Green }; open Color; match (Color.Green) { Red => 1, Green => 2 } }
