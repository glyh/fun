# match nested
{ type Option a = Some(a) | None; match (Some(Some(7))) { Some(Some(x)) => x, Some(None) => 0, None => 0 } }
