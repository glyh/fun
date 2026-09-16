# open struct constructors
{ Color = module { pub type Color = Red | Green | Blue }; open Color; match (Red) { Red => 1, Green => 2, Blue => 3 } }
