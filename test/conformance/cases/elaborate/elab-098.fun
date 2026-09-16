{ type Option A = Some A | None; rec List = fn[A : Type] { struct {meta: A; next: Option(List[A])} }; List{meta = 1; next = None} }
