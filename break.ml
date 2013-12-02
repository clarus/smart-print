type t =
  | Space
  | NewLine

let merge (b1 : t) (b2 : t) : t =
  match (b1, b2) with
  | Space, _ -> b2
  | _, Space -> b1
  | NewLine, NewLine -> b1