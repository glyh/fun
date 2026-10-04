# a Decl macro with an empty parameter list
{ M = module { macro four() : Decl { quote { four = 4 } }; four(); pub r = 1 }; M.r + 3 }
