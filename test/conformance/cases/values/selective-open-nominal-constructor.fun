# open T.{R} of a nominal type brings the named constructor
{ rec Color = enum { R, G }; open Color.{R}; match (R) { R => 1, _ => 2 } }
