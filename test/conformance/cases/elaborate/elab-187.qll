{ S = module { pub type Color = Red }; T = module { pub type Color = Red }; match (S.Red) { T.Red => 1, _ => 0 } }
