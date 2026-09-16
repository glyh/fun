# a saved continuation may outlive its handler
{ effect Async = sig { pause : Unit -> Unit }; q = ref(fn(u : Unit) { 0 }); _ = match (perform Async.pause(())) { v => 1, effect Async.pause _ => { q <- fn(u : Unit) { resume(()) }; 2 } }; 5 }
