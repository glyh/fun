# 7I generated syntax hygiene preserves introduced binders
{
       M = module {
         syntax let_x { let_x $body => { x = 1; $body } };
         pub result = {
           x = 99;
           let_x x
         }
       };
       M.result
     }
