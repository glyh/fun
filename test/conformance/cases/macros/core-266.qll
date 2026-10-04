# captured hole keeps use-site scope
{
       x = 1;
       syntax passthrough { passthrough $body => $body };
       {
         x = 99;
         passthrough x
       }
     }
