{ S = module { pub type Color = Red | Green }; T = module { pub type Color = Blue }; _ : T.Color = S.Red; () }
