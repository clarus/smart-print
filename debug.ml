(** A pretty-printer for the pretty-printer itself. *)
open Document
open Atom

(** Pretty-print an atom. *)
let rec pp (a : Atom.t) : Document.t =
  match a with
  | String s -> !^ "\"" ^-^ !^ s ^-^ !^ "\""
  | Break Break.Space -> !^ "Space"
  | Break Break.Newline -> !^ "Newline"
  | GroupOne (i, _as) -> !^ "GroupOne" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pps _as)
  | GroupAll (i, _as) -> !^ "GroupAll" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pps _as)

(** Pretty-print a list of atoms. *)
and pps (_as : t list) : Document.t =
  group_all (separate (!^ "," ^^ space) (List.map (fun a -> nest 2 (pp a)) _as))