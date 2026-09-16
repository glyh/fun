{ S = module { type Color = Red | Green }; match (S.Red) { S.Red => 1, _ => 0 } }
