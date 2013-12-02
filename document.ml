type t =
  | Empty
  | Leaf of Atom.t
  | Node of t * t

let empty : t = Empty

let string (s : string) : t =
  Leaf (Atom.String s)

let (!^) = string

let space : t = Leaf Atom.Space

let new_line : t = Leaf Atom.NewLine

let concat (d1 : t) (d2 : t) : t =
  Node (d1, d2)

let (^-^) = concat

let concat_with_space (d1 : t) (d2 : t) : t =
  concat d1 (concat space d2)

let (^^) = concat_with_space

let flatten (d : t) : Atom.t list =
  let rec aux (d : t) (l : Atom.t list) : Atom.t list =
    match d with
    | Empty -> l
    | Leaf a ->
      (match l with
      | [] -> [a]
      | a' :: l -> Atom.concat a a' @ l)
    | Node (d1, d2) -> aux d1 (aux d2 l) in
  aux d []

let nest (i : int) (d : t) : t =
  Leaf (Atom.GroupOne (i, flatten d))

let nest_all (i : int) (d : t) : t =
  Leaf (Atom.GroupAll (i, flatten d))

let group (d : t) : t =
  nest 0 d

let group_all (d : t) : t =
  nest_all 0 d

let parens (d : t) : t =
  !^ "(" ^-^ d ^-^ !^ ")"

let to_string (d : t) : string =
  let a = Atom.GroupOne (0, flatten d) in
  let (a, _) = Atom.eval 20 0 a 0 in
  Atom.to_string a
