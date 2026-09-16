# definition-site scope for template refs
{
       y = 2;
       syntax get_y { get_y => y };
       {
         y = 99;
         get_y
       }
     }
