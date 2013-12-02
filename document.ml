type t =
  | Empty
  | Leaf of Atom.t
  | Node of t * t

let empty : t = Empty

let string (s : string) : t =
  Leaf (Atom.String s)

let (!^) = string

let space : t = Leaf (Atom.Break Atom.Break.Space)

let new_line : t = Leaf (Atom.Break Atom.Break.NewLine)

let append (d1 : t) (d2 : t) : t =
  Node (d1, d2)

let (^-^) = append

let concat_with_space (d1 : t) (d2 : t) : t =
  d1 ^-^ space ^-^ d2

let (^^) = concat_with_space

let flatten (d : t) : Atom.t list =
  let rec aux (d : t) (l : Atom.t list) : Atom.t list =
    match d with
    | Empty -> l
    | Leaf a -> a :: l
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

let concat (ds : t list) : t =
  List.fold_left append empty ds

let separate (separator : t) (ds : t list) : t =
  let rec aux ds =
    match ds with
    | [] -> empty
    | d :: ds -> separator ^-^ d ^-^ aux ds in
  match ds with
  | [] -> empty
  | d :: ds -> d ^-^ aux ds

module OCaml = struct
  let int (i : int) : t =
    !^ (string_of_int i)
end

let to_string (d : t) : string =
  Atom.to_string (Atom.render 20 @@ flatten d)
