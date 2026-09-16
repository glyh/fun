# match wildcard
{ type Color = Red | Green | Blue; match (Green) { Red => 1, _ => 99 } }
