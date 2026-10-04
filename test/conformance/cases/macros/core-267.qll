# introduced binder captures introduced reference
{
       x = 1;
       syntax local_x { local_x => { x = 7; x } };
       {
         x = 99;
         local_x
       }
     }
