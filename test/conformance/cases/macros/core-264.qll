# hygiene: template binder does not capture use-site
{
       x = 1;
       syntax let_in {
       let_in $val $body => { x = $val; $body }
       };
       let_in 2 x
     }
