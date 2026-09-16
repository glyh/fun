# handler value branch handles same effect
{ effect Ask = sig { value : Unit -> I64 };
     match (0) { x => perform Ask.value(()),
     effect Ask.value () => 42
     }
     }
