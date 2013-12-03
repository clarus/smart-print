(** A pretty-printer for the pretty-printer itself. *)
open Document
open Atom

(** Pretty-print an atom. *)
let rec pp_atom (a : Atom.t) : Document.t =
  match a with
  | String s -> OCaml.string s
  | Break Break.Space -> !^ "Space"
  | Break Break.Newline -> !^ "Newline"
  | GroupOne (i, _as) -> !^ "GroupOne" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)
  | GroupAll (i, _as) -> !^ "GroupAll" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)

(** Pretty-print a list of atoms. *)
and pp_atoms (_as : Atom.t list) : Document.t =
  group_all (separate (!^ "," ^^ space) (List.map (fun a -> nest 2 (pp_atom a)) _as))

let pp_document (d : Document.t) : Document.t =
  OCaml.list pp_atom (Document.unsafe_to_atoms d)