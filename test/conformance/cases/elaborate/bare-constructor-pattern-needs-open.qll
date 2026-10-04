# a raw enum's constructors are members: a bare pattern head needs an open
{ Color = enum { Red, Green }; match (Color.Green) { Red => 1, Green => 2 } }
