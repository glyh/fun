{ Counter = module { pub count = ref(0); pub tick : Unit ->{Mutate(count)} Unit = fn(_) { count <- deref(count) + 1 } }; _ = Counter.tick(()); deref(Counter.count) }
