# extract expression from do block
{
       syntax extract {
       extract { $body } => $body
       };
       extract { 42 }
     }
