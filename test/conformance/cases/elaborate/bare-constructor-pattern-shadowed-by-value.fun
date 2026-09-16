# a bare pattern head resolves like any name: here to the value binder, not a constructor
{ Color = enum { Red, Green }; open Color; Red = 5; match (Color.Green) { Red => 1, _ => 2 } }
