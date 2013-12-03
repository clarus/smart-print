(* A document is a binary tree of atoms so that concatenation happens in O(1). *)
type t =
  | Empty
  | Leaf of Atom.t
  | Node of t * t

(* * Basics * *)
let empty : t = Empty

let string (s : string) : t =
  if s = "" then
    empty
  else
    Leaf (Atom.String (s, 0, String.length s))

let (!^) = string

let sub_string (s : string) (o : int) (l : int) : t =
  Leaf (Atom.String (s, o, l))

let space : t = Leaf (Atom.Break Break.Space)

let newline : t = Leaf (Atom.Break Break.Newline)

let append (d1 : t) (d2 : t) : t =
  Node (d1, d2)

let (^-^) = append

let concat_with_space (d1 : t) (d2 : t) : t =
  d1 ^-^ space ^-^ d2

let (^^) = concat_with_space

let unsafe_to_atoms (d : t) : Atom.t list =
  let rec aux (d : t) (l : Atom.t list) : Atom.t list =
    match d with
    | Empty -> l
    | Leaf a -> a :: l
    | Node (d1, d2) -> aux d1 (aux d2 l) in
  aux d []

(* * Indentation and grouping * *)
let nest (i : int) (d : t) : t =
  Leaf (Atom.GroupOne (i, unsafe_to_atoms d))

let nest_all (i : int) (d : t) : t =
  Leaf (Atom.GroupAll (i, unsafe_to_atoms d))

let group (d : t) : t =
  nest 0 d

let group_all (d : t) : t =
  nest_all 0 d

(* * Enclosing * *)
let parens (d : t) : t =
  !^ "(" ^-^ d ^-^ !^ ")"

let braces (d : t) : t =
  !^ "{" ^-^ d ^-^ !^ "}"

let brakets (d : t) : t =
  !^ "[" ^-^ d ^-^ !^ "]"

let angle_brakets (d : t) : t =
  !^ "<" ^-^ d ^-^ !^ ">"

let single_quotes (d : t) : t =
  !^ "'" ^-^ d ^-^ !^ "'"

let double_quotes (d : t) : t =
  !^ "\"" ^-^ d ^-^ !^ "\""

(* * Lists * *)
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

(* * Text * *)
let words (s : string) : t =
  group @@ separate space @@ List.map string @@ Str.split (Str.regexp "[ \t\n]") s

(* * OCaml values * *)
module OCaml = struct
  let bool (b : bool) : t =
    !^ (string_of_bool b)

  let int (i : int) : t =
    !^ (string_of_int i)

  let float (f : float) : t =
    !^ (string_of_float f)

  let string (s : string) : t =
    double_quotes (!^ (String.escaped s))

  let option (d : 'a -> t) (o : 'a option) : t =
    match o with
    | None -> !^ "None"
    | Some x -> !^ "Some" ^^ nest 2 (d x)

  let list (d : 'a -> t) (l : 'a list) : t =
    brakets (nest_all 2 @@ separate (!^ ";" ^^ space) (List.map d l))
end

(* * Rendering * *)
let to_buffer (width : int) (b : Buffer.t) (d : t) : unit =
  Atom.to_buffer b @@ Atom.render width @@ unsafe_to_atoms d

let to_string (width : int) (d : t) : string =
  let b = Buffer.create 10 in
  to_buffer width b d;
  Buffer.contents b

let to_out_channel (width : int) (c : out_channel) (d : t) : unit =
  Atom.to_out_channel c @@ Atom.render width @@ unsafe_to_atoms d

let to_stdout (width : int) (d : t) : unit =
  to_out_channel width stdout d