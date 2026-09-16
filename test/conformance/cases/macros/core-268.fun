# definition-site ref is not captured by lambda use site
{
       x = 1;
       syntax get_x { get_x => x };
       (fn(x) { get_x })(99)
     }
