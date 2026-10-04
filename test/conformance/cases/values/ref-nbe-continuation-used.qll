# a captured continuation is one-shot: a second resume is an evaluation error (Nbe.Effects.cs:123)
{ effect Ask = sig { ask : I64 -> I64 }; match (perform Ask.ask(1)) { n => n, effect Ask.ask k => { resume(1); resume(2) } } }
