# an unannotated recursive definition sees itself at a meta; defining it runs nothing
{ rec loop = fn(n) { loop(n) }; 1 }
