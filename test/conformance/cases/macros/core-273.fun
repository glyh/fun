# syntax template can expand to another syntax use
{
       syntax five { five => 5 };
       syntax call_five { call_five => five };
       call_five
     }
