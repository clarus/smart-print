open Document
open Atom

let rec pp (a : t) : Document.t =
  match a with
  | String s -> !^ "\"" ^-^ !^ s ^-^ !^ "\""
  | Break Break.Space -> !^ "Space"
  | Break Break.NewLine -> !^ "NewLine"
  | GroupOne (i, _as) -> !^ "GroupOne" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pps _as)
  | GroupAll (i, _as) -> !^ "GroupAll" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pps _as)

and pps (_as : t list) : Document.t =
  group_all (separate (!^ "," ^^ space) (List.map (fun a -> nest 2 (pp a)) _as))