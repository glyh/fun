let wrap_stx (stx : Syntax.t) : Core.value = Core.VStx stx

let unwrap_stx (v : Core.value) : Syntax.t option =
  match v with Core.VStx stx -> Some stx | _ -> None
